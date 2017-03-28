//
//  CyanifyOperation.swift
//  Button
//
//  Created by ReasonAmu on 3/23/17.
//  Copyright © 2017 RevolutionSport. All rights reserved.
//

import AVFoundation
import Dispatch

enum CyanifyError : Error {
    case noMediaData
}

class CyanifyOperation : Operation {
    
    
    
    //MARK: Types
    enum Result {
        case success
        case cancellation
        case failure(Error)
    }
    
    //MARK: Properties
    override var isExecuting: Bool {
        return result == nil
    }
    
    override var isFinished: Bool {
        return result != nil
    }
    
    var result : Result? {
        
        willSet {
            willChangeValue(forKey: "isExecuting")
            willChangeValue(forKey: "isFinished")
        }
        
        didSet {
            didChangeValue(forKey: "isExecuting")
            didChangeValue(forKey: "isFinished")
        }
    }
    
    private let asset: AVAsset // khai bao asset
    private let outputURL : URL! // url dau ra
    private var sampleTransferError : Error?
    
    
    //MARK: Khoi tao
    init(sourceURL : URL , outputURL : URL) {
        asset = AVAsset(url: sourceURL) // init AVAsset
        self.outputURL = outputURL
    }
    
    override var isAsynchronous: Bool {
        return true
    }
    
    
    //Moi 1 lan start phai goi finish 1 lan
    override func start() {
        guard  !isCancelled  else {
            finish(result: .cancellation)
            return
        }
        
        
        // tải asset properties ở chế độ nền, để tránh người dùng gọi bằng I/0 đồng bộ
        asset.loadValuesAsynchronously(forKeys: ["tracks"]) {
            guard !self.isCancelled else {
                self.finish(result: .cancellation)
                return
            }
            
            // Tất cả được khởi tạo ở khối lệnh bên dưới, eg : k có lỗi nào xảy ra
            let assetReader: AVAssetReader
            let assetWriter : AVAssetWriter
            let videoReaderOutputsAndWriterInputs : [ReaderOutputAndWriterInput]
            let passthroughReaderOutputsAndWriterInputs : [ReaderOutputAndWriterInput] // pass qua
            
            do {
                // Đảm bảo các asset (tệp tin ) tải thành công.
                
                var trackLoadingError : NSError?
                guard self.asset.statusOfValue(forKey: "tracks", error: &trackLoadingError) == .loaded else {
                    throw trackLoadingError!
                }
                let tracks = self.asset.tracks
                
                // Khởi tạo reader / writer
                assetReader = try AVAssetReader(asset: self.asset)
                assetWriter = try AVAssetWriter(url: self.outputURL, fileType: AVFileTypeQuickTimeMovie)
                
                let (videoReaderOutputs, passthroughReaderOutputs)  = try self.makeReaderOutputsForTracks(tracks: tracks, availableMediaTypes: assetWriter.availableMediaTypes)
                
                videoReaderOutputsAndWriterInputs = try self.makeVideoWriterInputsForVideoReaderOutputs(videoReaderOutputs: videoReaderOutputs)
                passthroughReaderOutputsAndWriterInputs = try self.makePassthroughWriterInputsForPassthroughReaderOutputs(passthroughReaderOutputs: passthroughReaderOutputs)
                
                // Hook everything up.
                
                for (readerOutput, writerInput) in videoReaderOutputsAndWriterInputs {
                    assetReader.add(readerOutput)
                    assetWriter.add(writerInput)
                }
                
                for (readerOutput, writerInput) in passthroughReaderOutputsAndWriterInputs {
                    assetReader.add(readerOutput)
                    assetWriter.add(writerInput)
                }
                
                /*
                 Remove file if necessary. AVAssetWriter will not overwrite
                 an existing file.
                 Xoá tệp nếu cần, AssetWriter không ghi đè lên 1 file đã có
                 */
                
                let fileManager = FileManager()
                if let outputPath: String = self.outputURL.path, fileManager.fileExists(atPath: outputPath) {
                    try fileManager.removeItem(at: self.outputURL)
                }
                
                // Start reading/writing.
                
                guard assetReader.startReading() else {
                    // `error` is non-nil when startReading returns false.
                    throw assetReader.error!
                }
                
                guard assetWriter.startWriting() else {
                    // `error` is non-nil when startWriting returns false.
                    throw assetWriter.error!
                }
                
                assetWriter.startSession(atSourceTime: kCMTimeZero)
                
            }
            catch {
                self.finish(result: .failure(error))
                return
            }
            
            let writingGroup = DispatchGroup()
            
            // Truyền dữ liệu tập tin đầu vào sang đầu ra
            self.transferVideoTracks(videoReaderOutputsAndWriterInputs: videoReaderOutputsAndWriterInputs, group: writingGroup)
            self.transferPassthroughTracks(passthroughReaderOutputsAndWriterInputs: passthroughReaderOutputsAndWriterInputs, group: writingGroup)
            
            // Xử lý hoàn thành
            let queue = DispatchQueue.global(priority: DispatchQueue.GlobalQueuePriority.default)
            writingGroup.notify(queue: queue, execute: {
                self.readingAndWritingDidFinish(assetReader: assetReader, assetWriter: assetWriter)
            })
        }
    }
    
    
    func finish(result: Result) {
        self.result = result
    }
    
    
    
    /*
     Tự đinh danh 1 kiểu dữ liệu riêng để có thể sử dụng tương ứng vs 1 'AVAssetWriterInput' cùng với 'AVAssetReaderInput'
     */
    fileprivate typealias ReaderOutputAndWriterInput = (readerOutput: AVAssetReaderOutput, writerInput: AVAssetWriterInput)
    
    //MARK: Chuyển hoá đầu ra cho ReaderOutputTrack
    private func makeReaderOutputsForTracks(tracks : [AVAssetTrack] , availableMediaTypes : [String] ) throws ->
        (videoReaderOutputs : [AVAssetReaderTrackOutput] , passthroughReaderOutputs : [AVAssetReaderTrackOutput] ) {
            // giải nén video thành source 32ARGB
            let videoDecompressionSettings : [String : Any] = [kCVPixelBufferPixelFormatTypeKey as String : NSNumber(value: kCVPixelFormatType_32ARGB),
                                                               String(kCVPixelBufferIOSurfacePropertiesKey) : [:]] // có thể thêm các thuộc tính width , height video khi xuất ra
            
            // Phân vùng theo dõi các track "video" vs "passthrough", tạo output cho Reader
            
            var videoReaderOutputs = [AVAssetReaderTrackOutput]()
            var passthroughReaderOutputs = [AVAssetReaderTrackOutput]()
            
            for track in tracks {
                guard availableMediaTypes.contains(track.mediaType) else { continue }
                
                switch track.mediaType {
                case AVMediaTypeVideo:
                    let videoReaderOutput = AVAssetReaderTrackOutput(track: track, outputSettings: videoDecompressionSettings)
                    videoReaderOutputs += [videoReaderOutput] // cong them mang
                default:
                    // nil output ->> passthrough
                    let passthroughReaderOutput = AVAssetReaderTrackOutput(track: track, outputSettings: nil)
                    passthroughReaderOutputs += [passthroughReaderOutput]
                }
            }
            
            return (videoReaderOutputs, passthroughReaderOutputs)
    }
    
    //MARK: Lấy AVAssetReaderTrackOutput ---> ReaderOutputAndWriterInput
    private func makeVideoWriterInputsForVideoReaderOutputs(videoReaderOutputs: [AVAssetReaderTrackOutput])  throws -> [ReaderOutputAndWriterInput] {
        // Nén source frame đã đổi thành H.264
        let videoCompressionSettings : [String: Any] = [AVVideoCodecKey : AVVideoCodecH264]
        /*
         In order to find the source format we need to create a temporary asset
         reader, plus a temporary track output for each "real" track output.
         We will only read as many samples (typically just one) as necessary
         to discover the format of the buffers that will be read from each "real"
         track output.
         
         Để tìm dạng định dạng nguồn, chúng ta cần phải tạo ra 1 asserReader tạm thời, cộng vs 1 kq theo dõi tạm thời cho môi
         đầu ra theo thời gian thực.
         */
        
        let tempAssetReader = try AVAssetReader(asset: asset)
        
        let videoReaderOutputsAndTempVideoReaderOutputs: [(videoReaderOutput: AVAssetReaderTrackOutput, tempVideoReaderOutput: AVAssetReaderTrackOutput)] = videoReaderOutputs.map { videoReaderOutput in
            let tempVideoReaderOutput = AVAssetReaderTrackOutput(track: videoReaderOutput.track, outputSettings: videoReaderOutput.outputSettings)
            
            tempAssetReader.add(tempVideoReaderOutput)
            
            return (videoReaderOutput, tempVideoReaderOutput)
        }
        
        // Start Reading
        
        guard tempAssetReader.startReading() else {
            // error khi start fail
            print("🔴", "Don't start")
            throw tempAssetReader.error!
        }
        
        // Tạo video AssetWriter input , bằng các source đầu ra của reader tạm thời
        var videoReaderOutputsAndWriterInputs = [ReaderOutputAndWriterInput]()
        
        for (videoReaderOutput, tempVideoReaderOutput) in videoReaderOutputsAndTempVideoReaderOutputs {
            // lấy đinh dạng source bộ đệm mẫu
            
            var videoFormatHint: CMFormatDescription?
            
            while videoFormatHint == nil {
                guard let sampleBuffer = tempVideoReaderOutput.copyNextSampleBuffer() else {
                    // ra khỏi bộ nhớ đệm mẫu trước khi tìm thấy 1 mô tả định dạng
                    throw CyanifyError.noMediaData
                }
                
                videoFormatHint = CMSampleBufferGetFormatDescription(sampleBuffer)
            }
            
            // Create asset writer input.
            let videoWriterInput = AVAssetWriterInput(mediaType: AVMediaTypeVideo, outputSettings: videoCompressionSettings, sourceFormatHint: videoFormatHint)
            
            videoReaderOutputsAndWriterInputs.append((readerOutput: videoReaderOutput, writerInput: videoWriterInput))
        }
        
        // Đóng các tiến trình xử lý, vì chỉ đọc 1 tập con của các mẫu
        tempAssetReader.cancelReading()
        
        return videoReaderOutputsAndWriterInputs
        
    }
    
    //MARK: AVAssetReaderTrackOutput ---> [ReaderOutputAndWriterInput]
    private func makePassthroughWriterInputsForPassthroughReaderOutputs(passthroughReaderOutputs : [AVAssetReaderTrackOutput]) throws -> [ReaderOutputAndWriterInput] {
        // Tạo các đầu vào của WriterInputs, sử dụng source track's theo WriterInput
        
        var passthroughReaderOutputsAndWriterInputs = [ReaderOutputAndWriterInput]()
        
        for passthroughReaderOutput in passthroughReaderOutputs {
            /*
             For passthrough, we can simply ask the track for its format
             description and use that as the writer input's format hint.
             -> Đối với passthrough, yêu cầu theo dõi để mô tả định dạng vs sử dụng Writer Input fomat
             */
            
            let trackFormatDescriptions = passthroughReaderOutput.track.formatDescriptions as! [CMFormatDescription]
            
            guard let passthroughFormatHint = trackFormatDescriptions.first else {
                throw CyanifyError.noMediaData
            }
            
            //Tạo AssetWriterInput với nil (passthrough) output setting
            let passthroughWriterInput = AVAssetWriterInput(mediaType: passthroughReaderOutput.mediaType, outputSettings: nil, sourceFormatHint: passthroughFormatHint)
            
            passthroughReaderOutputsAndWriterInputs.append((readerOutput: passthroughReaderOutput, writerInput: passthroughWriterInput))
        }
        
        return passthroughReaderOutputsAndWriterInputs
    }
    
    //MARK: append view lên video
    private func transferVideoTracks(videoReaderOutputsAndWriterInputs: [ReaderOutputAndWriterInput], group : DispatchGroup) {
        for (videoReaderOutput, videoWriterInput) in videoReaderOutputsAndWriterInputs {
            let perTrackDispatchQueue = DispatchQueue(label: "Track data transfer queue: \(videoReaderOutput) -> \(videoWriterInput).", attributes: [])
            // tạo 1 block để thay đổi màu trên mỗi frame
            
            let videoProcessor: (CMSampleBuffer) throws -> Void = { sampleBuffer in
                if let imageBuffer = CMSampleBufferGetImageBuffer(sampleBuffer),
                    let pixelBuffer: CVPixelBuffer = imageBuffer, CFGetTypeID(imageBuffer) == CVPixelBufferGetTypeID() {
                    
                    let redComponentIndex = 1
                    try pixelBuffer.removeARGBColorComponentAtIndex(componentIndex: redComponentIndex)
                }
            }
            
            group.enter()
            transferSamplesAsynchronouslyFromReaderOutput(videoReaderOutput,
                                                          toWriterInput: videoWriterInput,
                                                          onQueue: perTrackDispatchQueue,
                                                          sampleBufferProcessor: videoProcessor) {
                                                            group.leave()
            }
            
        }
    }
    
    //MARK: Truyền Track qua ReadOutput - WriterInput:
    private func transferPassthroughTracks(passthroughReaderOutputsAndWriterInputs : [ReaderOutputAndWriterInput], group : DispatchGroup) {
        
        for (passthroughReaderOutput, passthroughWriterInput) in passthroughReaderOutputsAndWriterInputs {
            let perTrackDispatchQueue = DispatchQueue(label: "Track data transfer queue: \(passthroughReaderOutput) -> \(passthroughWriterInput).", attributes: [])
            
            group.enter()
            transferSamplesAsynchronouslyFromReaderOutput(passthroughReaderOutput,
                                                          toWriterInput: passthroughWriterInput,
                                                          onQueue: perTrackDispatchQueue) {
                                                          group.leave()
            }
        }
    }
    
    
    //MARK: Truyền Samples Buffer Pixel không đồng bộ từ ReaderOutput --> WriterInput
    private func transferSamplesAsynchronouslyFromReaderOutput(_ readerOutput: AVAssetReaderOutput,
                                                               toWriterInput writerInput: AVAssetWriterInput,
                                                               onQueue queue: DispatchQueue,
                                                               sampleBufferProcessor: ((_ sampleBuffer: CMSampleBuffer) throws -> Void)? = nil, completionHandler: @escaping (Void) -> Void) {
        // Cung cấp đầu vào AssetWriterInput cùng với 1 block call bất kì lúc nào cần samples
          writerInput.requestMediaDataWhenReady(on: queue) {
            var isDone = false
            /*
             Loop, transferring one sample per iteration, until the asset writer
             input has enough samples. At that point, exit the callback block
             and the asset writer input will invoke the block again when it
             needs more samples.
             
             Chuyển 1 sample mỗi 1 lần lặp cho đến khi AssetWriterInput có đủ samples
             Tại thời điểm đó, thoát khỏi block và AssetWriterInput sẽ gọi lại block khi nó cần thêm nhiều sample hơn
             */
            
            while writerInput.isReadyForMoreMediaData {
                guard !self.isCancelled else {
                    isDone = true
                    break
                }
            }
            
            // Lấy sample tiếp theo từ AssetReaderOutput
             guard let sampleBuffer = readerOutput.copyNextSampleBuffer() else {
                //  Tại thời điểm này, AssetReaderOutput không có sample để chuyển đổi nữa
                isDone = true
                break
             }
            
            // Xử lý sample, nếu được yêu cầu
            do {
                try sampleBufferProcessor?(sampleBuffer)
            }
            catch {
                // Lỗi này sẽ được thấy trong readingAndWritingDidFinish
                self.sampleTransferError = error
                isDone = true
            }
            
            // Append the sample to the asset writer input. -- Nối các sample vào trong AssetWriterInput
            guard writerInput.append(sampleBuffer) else {
                // Lỗi nếu không thêm được sampleBuffer hiển thị trong ReadAndWritingDidFinish
                isDone = true
                break
            }
            
            if isDone {
                /*
                 Calling `markAsFinished()` on the asset writer input will both:
                 1. Unblock any other inputs that need more samples.
                 2. Cancel further invocations of this "request media data"
                 callback block.
                 
                 Gọi markAsFinished để bắt đầu trình ghi vào AssetWriterInput :
                 1. bỏ chặn bất kì đầu vào nào khác cần thêm sample
                 2. huỷ các lệnh gọi thêm block "request media data" này
                 */
                
                writerInput.markAsFinished()
                
                // Block đã thực hiện chuyển dữ liệu
                completionHandler()
            }
        }
    }
    
    
    //MARK: Reading - Writing hoàn thành
    private func readingAndWritingDidFinish (assetReader : AVAssetReader , assetWriter : AVAssetWriter) {
        if isCancelled {
            assetReader.cancelReading()
            assetWriter.cancelWriting()
        }
        
        // Xử lý bất kì lỗi nào trong quá trình render video
        guard sampleTransferError == nil else {
            assetReader.cancelReading()
            assetWriter.cancelWriting()
            finish(result: .failure(sampleTransferError!))
            return
        }
        
        // Đánh giá result của việc reader sample
        guard assetReader.status == .completed else {
            let result: Result
            
            switch assetReader.status {
            case .cancelled:
                assetWriter.cancelWriting()
                result = .cancellation
                
            case .failed:
                // `error` property is non-nil in the `.Failed` status.
                result = .failure(assetReader.error!)
                
            default:
                fatalError("Unexpected terminal asset reader status: \(assetReader.status).")
            }
            
            finish(result: result)
            
            return
        }
        
        
        // Finish writing không đồng bộ đánh giá result writing sample
        assetWriter.finishWriting {
            let result: Result
            
            switch assetWriter.status {
            case .completed:
                result = .success
                
            case .cancelled:
                result = .cancellation
                
            case .failed:
                // `error` property is non-nil in the `.Failed` status.
                result = .failure(assetWriter.error!)
                
            default:
                fatalError("Unexpected terminal asset writer status: \(assetWriter.status).")
            }
            
            self.finish(result: result)
        }
    }
}

extension CVPixelBuffer {
    /**
     Iterates through each pixel in the receiver (assumed to be in ARGB format)
     and overwrites the color component at the given index with a zero. This
     has the effect of "cyanifying," "rosifying," etc (depending on the chosen
     color component) the overall image represented by the pixel buffer.
     */
    func removeARGBColorComponentAtIndex(componentIndex: size_t) throws {
        let lockBaseAddressResult = CVPixelBufferLockBaseAddress(self, CVPixelBufferLockFlags(rawValue: CVOptionFlags(0)))
        
        guard lockBaseAddressResult == kCVReturnSuccess else {
            throw NSError(domain: NSOSStatusErrorDomain, code: Int(lockBaseAddressResult), userInfo: nil)
        }
        
        let bufferHeight = CVPixelBufferGetHeight(self)
        
        let bufferWidth = CVPixelBufferGetWidth(self)
        
        let bytesPerRow = CVPixelBufferGetBytesPerRow(self)
        
        let bytesPerPixel = bytesPerRow / bufferWidth
        
        if let baseAddress = CVPixelBufferGetBaseAddress(self) {
            let base = baseAddress.assumingMemoryBound(to: UInt8.self)
            // `buf` is `UnsafeMutablePointer<UInt8>`
            // For each pixel, zero out selected color component.
            for row in 0..<bufferHeight {
                for column in 0..<bufferWidth {
                    let pixel = base + (row * bytesPerRow) + (column * bytesPerPixel)
                    pixel[componentIndex] = 0
                }
            }
            
            let unlockBaseAddressResult = CVPixelBufferUnlockBaseAddress(self, CVPixelBufferLockFlags(rawValue: CVOptionFlags(0)))
            
            guard unlockBaseAddressResult == kCVReturnSuccess else {
                throw NSError(domain: NSOSStatusErrorDomain, code: Int(unlockBaseAddressResult), userInfo: nil)
            }
            
        }
        
        
    }
}


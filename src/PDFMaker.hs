module PDFMaker where

import AST
import Graphics.PDF

makePDF :: Document -> String -> IO ()
makePDF doc name = do
  let rect = PDFRect 0 0 2480 3508
  runPdf name (standardDocInfo { author=toPDFString "PDFMaker", compressed = False}) rect $ do
      page1 <- addPage Nothing
      makeDocument page1 doc

makeDocument :: PDFReference PDFPage -> Document -> PDF () 
makeDocument page doc = drawWithPage page $ makeSection doc (upperBound - padding) (lowerBound + padding) (leftBound + padding) (rightBound - padding)

-- Falta imprimir titulo, restr
makeSection :: Document -> PDFFloat -> PDFFloat -> PDFFloat -> PDFFloat -> Draw ()
makeSection (Section t (Options res opts)) upb lob leb rib = do
  let lowerLeft = (leb) :+ (lob )
  let upperRight = (rib ) :+ (upb)
  strokeColor black
  setWidth lineW
  stroke $ Rectangle lowerLeft upperRight
  let nopts = (fromIntegral $ length opts) :: PDFFloat
  let totH = (upb - lob)
  let optH = (totH - padding * (nopts + 1)) / nopts
  let totW = (rib - leb)
  let optW = totW - 4 * padding
  let firstUpb = upb - padding 
  let firstLeb = leb + padding
  makeOptions opts firstUpb firstLeb optH optW

makeSection (Section t (Subs subs)) upb lob leb rib = do
  let lowerLeft = (leb) :+ (lob )
  let upperRight = (rib ) :+ (upb)
  strokeColor black
  setWidth lineW
  stroke $ Rectangle lowerLeft upperRight
  let nsubs = (fromIntegral $ length subs) :: PDFFloat
  let totH = (upb - lob)
  let subH = (totH - padding * (nsubs + 1)) / nsubs
  let totW = (rib - leb)
  let subW = totW - 2 * padding
  let firstUpb = upb - padding 
  let firstLeb = leb + padding
  makeSubsections subs firstUpb firstLeb subH subW

makeSubsections :: [Document] -> PDFFloat -> PDFFloat -> PDFFloat -> PDFFloat -> Draw ()
makeSubsections [] _ _ _ _ = return ()
makeSubsections (doc : docs) upb leb h w = do
  makeSection doc upb (upb - h) leb (leb + w)
  makeSubsections docs (upb - h - padding) leb h w

makeOptions :: [Option] -> PDFFloat -> PDFFloat -> PDFFloat -> PDFFloat -> Draw ()
makeOptions [] _ _ _ _ = return ()
makeOptions (opt : opts) upb leb h w = do
  displayFormattedText  (Rectangle (leb :+ (upb - h)) ((leb + w) :+ upb)) 
                        NormalParagraph 
                        (Font (PDFFont Helvetica fontSize) black black) $ do 
                          paragraph $ do
                            txt $ opt
  strokeColor black
  setWidth lineW
  stroke $ Circle (leb + w + 2 * padding) (upb - (circleSize)) (min circleSize h)
  makeOptions opts (upb - h - padding) leb h w









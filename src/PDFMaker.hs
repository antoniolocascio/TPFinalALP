module PDFMaker (makePDF) where

import Types
import Graphics.PDF

padding :: PDFFloat
padding = 100

lineW = 10
fontSize :: Int
fontSize = 40
circleSize = 40

-- A4 default size
upperBound = 3508
lowerBound = 0
leftBound = 0
rightBound = 2480

-- Crea el PDF en base a la descripcion del documento
makePDF :: Document -> String -> IO ()
makePDF doc name = do
  let rect = PDFRect 0 0 2480 3508
  runPdf name (standardDocInfo { author=toPDFString "PDFMaker", compressed = False}) rect (makeDocument doc)

-- Crea el contenido del PDF
makeDocument :: Document -> PDF ()
makeDocument [] = return ()
makeDocument (pg:pgs) = do
  page <- addPage Nothing 
  makePage page pg
  makeDocument pgs

-- Genera una pagina del documento
makePage :: PDFReference PDFPage -> Page -> PDF () 
makePage page doc = drawWithPage page $ makeSection doc (upperBound - padding) (lowerBound + padding) (leftBound + padding) (rightBound - padding)

-- Genera una seccion de una pagina
makeSection :: Page -> PDFFloat -> PDFFloat -> PDFFloat -> PDFFloat -> Draw ()
makeSection (Section t (Options res opts)) upb lob leb rib = do
  let lowerLeft = leb :+ lob 
      upperRight = rib :+ upb
  strokeColor black
  setWidth lineW
  stroke $ Graphics.PDF.Rectangle lowerLeft upperRight
  makeText t leb (upb + padding / 4)
  makeRestriction res (leb + padding / 4) (upb - (fromIntegral fontSize)) 
  let nopts = (fromIntegral $ length opts) :: PDFFloat
      totH = (upb - lob)
      optH = (totH - padding * (nopts + 1)) / nopts
      totW = (rib - leb)
      optW = totW - 4 * padding
      firstUpb = upb - padding 
      firstLeb = leb + padding
  makeOptions opts firstUpb firstLeb optH optW

makeSection (Section t (Subs subs)) upb lob leb rib = do
  let lowerLeft = leb :+ lob
      upperRight = rib :+ upb
  strokeColor black
  setWidth lineW
  stroke $ Graphics.PDF.Rectangle lowerLeft upperRight
  makeText t leb (upb + padding / 4)
  let nsubs = (fromIntegral $ length subs) :: PDFFloat
      totH = (upb - lob)
      subH = (totH - padding * (nsubs + 1)) / nsubs
      totW = (rib - leb)
      subW = totW - 2 * padding
      firstUpb = upb - padding 
      firstLeb = leb + padding
  makeSubsections subs firstUpb firstLeb subH subW


-- Agrega el texto correspondiente a la restriccion de opciones
makeRestriction :: Restriction -> PDFFloat -> PDFFloat -> Draw ()
makeRestriction res x y = makeText (showRestr res) x y 
  where
    showRestr True  = "Mark only one option"
    showRestr False = "Mark one or more"

-- Agrega texto en una posicion
makeText :: String -> PDFFloat -> PDFFloat -> Draw ()
makeText  str x y = drawText $ do
          setFont (PDFFont Helvetica fontSize)
          textStart x y
          renderMode FillText
          displayText $ toPDFString str

-- Genera las subsecciones de una seccion
makeSubsections :: [Page] -> PDFFloat -> PDFFloat -> PDFFloat -> PDFFloat -> Draw ()
makeSubsections [] _ _ _ _ = return ()
makeSubsections (doc : docs) upb leb h w = do
  makeSection doc upb (upb - h) leb (leb + w)
  makeSubsections docs (upb - h - padding) leb h w

-- Genera las opciones, dada una lista
makeOptions :: [Option] -> PDFFloat -> PDFFloat -> PDFFloat -> PDFFloat -> Draw ()
makeOptions [opt] upb leb h w = makeOption opt upb leb h w True
makeOptions (opt : opts) upb leb h w = do
  makeOption opt upb leb h w False
  makeOptions opts (upb - h - padding) leb h w

-- Genera una opcion
makeOption :: Option -> PDFFloat -> PDFFloat -> PDFFloat -> PDFFloat -> Bool -> Draw ()
makeOption opt upb leb h w last = do
  displayFormattedText  (Graphics.PDF.Rectangle (leb :+ (upb - h)) ((leb + w) :+ upb)) 
                        NormalParagraph 
                        (Font (PDFFont Helvetica fontSize) black black) $ do 
                          paragraph $ do
                                        txt $ opt
  strokeColor black
  setWidth lineW
  stroke $ Graphics.PDF.Circle (leb + w + 2 * padding) (upb - (h / 2)) (min circleSize (2*h))
  if last 
    then return ()
    else do setWidth (lineW / 3)
            strokeColor black
            setDash $ DashPattern [25] 25
            let ypos = (upb - h - padding / 3)
            beginPath ((leb + padding):+ypos)
            addLineToPath ((leb + w + padding):+ypos)
            closePath
            strokePath
            setNoDash
            





module PDFMaker where

import AST
import Graphics.PDF

makePDF :: IO()
makePDF = do
    let rect = PDFRect 0 0 2480 3508
    runPdf "demo.pdf" (standardDocInfo { author=toPDFString "alpheccar", compressed = False}) rect $ do
        myDocument

myDocument :: PDF () 
myDocument = do
    page1 <- addPage Nothing
    createPageContent page1

createPageContent :: PDFReference PDFPage -> PDF () 
createPageContent page = drawWithPage page $ do 
    strokeColor black
    setWidth 10
    beginPath (0:+0)
    addLineToPath (0:+400)
    addLineToPath (400:+400)
    addLineToPath (400:+0)
    closePath
    strokePath
    moveto (100:+100)
    stroke $ Rectangle 300 300

--Color, stroke width, punto sup izq, punto inf der
strokeRect :: Color -> PDFFloat -> Point -> Point -> Draw ()
stroke c w p1 p2 = do
  strokeColor c
  setWidth w
  beginPath p1
  addLineToPath
open System.Reflection
open System.Net.NetworkInformation

//
// F# program to generate random Mondrian Art.
//
// Jhon Nunez
// U. of Illinois, Chicago
// CS 341, Spring 2018
// Project #05
//

#light

//------------------------------------------------------------------------------------------------------
// randomInt LB UB
//
// generates random integer in range LB..UB, inclusive.
//
// NOTE: if you want repeatable random numbers for testing,
// uncomment "let seed = 0".  If you want random images 
// every time, uncomment the other "let seed = ..." line.
//
let seed = System.DateTime.Now.Millisecond
//let seed = 0
let ranInt = new System.Random(seed)

let randomInt LB UB =
  if LB <= UB then
    ranInt.Next(LB, UB+1)
  else
    LB

//------------------------------------------------------------------------------------------------------
//
// randomRect
//
// An example of generating a random-colored rectangle
// in HTML SVG format.
//
let randomRect x1 y1 x2 y2 = 
  
  let randomNumber = randomInt 0 100
  
  if (randomNumber < 9)
    then 
    let red = 255
    let green = 0
    let blue = 0
    let html = 
       "<rect x=" + (string x1) +
       " y=" + (string y1) + 
       " width=" + (string (x2-x1+1)) + 
       " height=" + (string (y2-y1+1)) + 
       " stroke=black"+" stroke-width = 4" +
       " fill=\"rgb(" + 
       (string red) + "," +
       (string green) + "," +
       (string blue) + ")\" />\n"
  
    html
  else if (randomNumber < 17)
    then
    let red = 135
    let green = 206
    let blue = 250
    let html = 
       "<rect x=" + (string x1) +
       " y=" + (string y1) + 
       " width=" + (string (x2-x1+1)) + 
       " height=" + (string (y2-y1+1)) + 
       " stroke=black"+" stroke-width = 4" +
       " fill=\"rgb(" + 
       (string red) + "," +
       (string green) + "," +
       (string blue) + ")\" />\n"
  
    html
  else if (randomNumber < 25)
    then
    let red = 255
    let green = 255
    let blue = 0
    let html = 
       "<rect x=" + (string x1) +
       " y=" + (string y1) + 
       " width=" + (string (x2-x1+1)) + 
       " height=" + (string (y2-y1+1)) + 
       " stroke=black"+" stroke-width = 4" +
       " fill=\"rgb(" + 
       (string red) + "," +
       (string green) + "," +
       (string blue) + ")\" />\n"
  
    html
  else 
    let red = 255
    let green = 255
    let blue = 255
    let html = 
       "<rect x=" + (string x1) +
       " y=" + (string y1) + 
       " width=" + (string (x2-x1+1)) + 
       " height=" + (string (y2-y1+1)) + 
       " stroke=black"+" stroke-width = 4" +
       " fill=\"rgb(" + 
       (string red) + "," +
       (string green) + "," +
       (string blue) + ")\" />\n"
  
    html


  //let red = randomInt 0 255
  //let green = randomInt 0 255
  //let blue = randomInt 0 255
  
//  let html = 
//       "<rect x=" + (string x1) +
//       " y=" + (string y1) + 
//       " width=" + (string (x2-x1+1)) + 
//       " height=" + (string (y2-y1+1)) + 
//       " stroke=black"+" stroke-width = 4" +
//       " fill=\"rgb(" + 
//       (string red) + "," +
//       (string green) + "," +
//       (string blue) + ")\" />\n"
  
 // html

//------------------------------------------------------------------------------------------------------
//Initial split function before stack overflow error
let split n = 
  
  let n1 = (n/3)
  let n2 = ( (n*2)/3 )

  let test = randomInt n1 n2
  test
//------------------------------------------------------------------------------------------------------
//Initial second split function before stack overflow error
let splitDecide canvasDimension =
 
 let decision = randomInt 120 ( (3*(canvasDimension) )/ 2)
 decision

//------------------------------------------------------------------------------------------------------

let multiply n = 

  let value = n * 1.5
  ( int(value) )

//
// _mondrian x1 y1 x2 y2 canvasWidth canvasHeight
//
// Recursive helper function that randomly generates an image
// for the area denoted by the rectange (x1,y1) and (x2,y2),
// where (x1,y1) is the upper-left corner and (x2,y2) is the 
// lower-right corner.  The image is in HTML SVG format.
//
let rec _mondrian x1 y1 x2 y2 canvasWidth canvasHeight  = 
   
  //let html = randomRect x1 y1 x2 y2
//  let html = randomRect 0 0 0 0 //white cavas
  //let test = List.map (fun x -> x ) html
  
  
  //let divide1 = split (x2 - x1)
  let divide1 = randomInt (x1 + (x2 - x1)/3) (x1 +  ( ( (x2-x1) * 2 ) / 3 ) )
  let divide2 = randomInt (y1 + (y2 - y1)/3) (y1 +  ( ( (y2-y1) * 2 ) / 3 ) )
  
//  let widthSplit = splitDecide canvasWidth 
//  let lengthSplit = splitDecide canvasHeight

  if ( (x2 - x1) > (canvasWidth/2) && (y2 - y1) > (canvasHeight/2) ) 
    then 
    let a = _mondrian x1 y1 divide1 y2 canvasWidth canvasHeight 
    let b = _mondrian divide1 y1 x2 y2 canvasWidth canvasHeight 
    let c = _mondrian x1 y1 x2 divide2 canvasWidth canvasHeight
    let d = _mondrian x1 divide2 x2 y2 canvasWidth canvasHeight
    a + b + c + d

  else if ( (x2 - x1) > (canvasWidth/2) ) 
    then 
    let e = _mondrian x1 y1 divide1 y2 canvasWidth canvasHeight
    let f = _mondrian divide1 y1 x2 y2 canvasWidth canvasHeight
    e + f

  else if ( (y2 - y1) > (canvasHeight/2) )
    then
    let g = _mondrian x1 y1 x2 divide2 canvasWidth canvasHeight
    let h = _mondrian x1 divide2 x2 y2 canvasWidth canvasHeight
    g + h

  //else if (widthSplit < canvasWidth && lengthSplit < canvasHeight)
  //  then
  //  let i = _mondrian x1 y1 divide1 y2 canvasWidth canvasHeight
  //  let j = _mondrian divide1 y1 x2 y2 canvasWidth canvasHeight
  //  let k = _mondrian x1 divide2 divide1 y2 canvasWidth canvasHeight
  //  let l = _mondrian divide1 divide2 x2 y2 canvasWidth canvasHeight
  //  i + j + k + l

  //else if (widthSplit < canvasWidth)
  //  then 
  //  let m = _mondrian x1 y1 divide1 y2 canvasWidth canvasHeight
  //  let n = _mondrian divide1 y1 x2 y2 canvasWidth canvasHeight
  //  m + n

 // else if (widthSplit < canvasHeight)
 //   then
 //   let o = _mondrian x1 divide2 divide1 y2 canvasWidth canvasHeight
 //   let p = _mondrian divide1 divide2 x2 y2 canvasWidth canvasHeight
 //   o + p

  else //Professor's suggestion:
    
    let widthSplit = randomInt 120 (multiply ( float(x2 - x1) ) )
    //splitDecide (x2 - x1)
    let lengthSplit = randomInt 120 (multiply ( float(y2 - y1) ) )

    if (widthSplit < (x2 - x1) && lengthSplit < (y2 - y1) )
      then
      let i = _mondrian x1 y1 divide1 y2 canvasWidth canvasHeight 
      let j = _mondrian divide1 y1 x2 y2 canvasWidth canvasHeight
      let k = _mondrian x1 y1 x2 divide2 canvasWidth canvasHeight
      let l = _mondrian x1 divide2 x2 y2 canvasWidth canvasHeight
      i + j + k + l
    
    else if (widthSplit < (x2 - x1) )
      then
      let m = _mondrian x1 y1 divide1 y2 canvasWidth canvasHeight
      let n = _mondrian divide1 y1 x2 y2 canvasWidth canvasHeight
      m + n

    else if (widthSplit < (y2 - y1))
      then
      let o = _mondrian x1 y1 x2 divide2 canvasWidth canvasHeight
      let p = _mondrian x1 divide2 x2 y2 canvasWidth canvasHeight
      o + p
    
    else
       let finalImage = randomRect x1 y1 x2 y2 
       finalImage
    
 


//------------------------------------------------------------------------------------------------------
//
// mondrian canvasWidth canvasHeight
//
// Randomly generates an image in the spirit of Piet Mondrian.
// Returns an HTML document containing an SVG image of the given
// canvas width and height.  
//
// SVG: https://www.w3schools.com/html/html5_svg.asp
//
let mondrian canvasWidth canvasHeight = 
  let prefix = "<html>\n<head></head>\n<body>\n" +
               "<svg width=\"" + (string canvasWidth) + 
               "\" height=\"" + (string canvasHeight) + "\">\n"
  

  let image = _mondrian 0 0 (canvasWidth-1) (canvasHeight-1) canvasWidth canvasHeight 
  //
  let suffix = "</svg>\n</body>\n</html>\n"
  let html = prefix + image + suffix
  html

// main: ------------------------------------------------------------------------------------------------

[<EntryPoint>]
let main argv =
  printfn "** Starting **"
  
  let width = 1024
  let height = 768
  let filename = "..\\..\\..\\mondrian.html"  // same folder as F# code:
  
  printfn "** Generating image... "
  let html = mondrian width height
  System.IO.File.WriteAllText(filename, html) 
  
  printfn "** Done **"
  0

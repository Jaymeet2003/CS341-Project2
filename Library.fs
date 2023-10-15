// Work on your solution here

// Do not change the API (module/namespace/function signatures)

// Refer to the PDF for more instructions on how to implement
// each function

// As of now, each function simply takes an image as input
// and returns it as it is

//
// Project Name ??
// Project Description ??
// Name : Jimmy Patel
// Netid :jpate289
// Date : 10/15/2023
//


namespace ImageLibrary

module Operations =
 
  let rec Grayscale (width:int) 
                    (height:int) 
                    (depth:int) 
                    (image:(int*int*int) list list) = 
    // This function calculates the grayscale value for a single pixel.
    let grayscalePixel (r, g, b: int): (int * int * int) =
        let gray = int ((float r * 0.299) + (float g * 0.587) + (float b * 0.114))
        (gray, gray, gray)


        
    match image with
    | [] -> []  // If the image list is empty, return an empty list.
    | row :: restOfImage -> 
        let grayRow = row |> List.map grayscalePixel // Convert the row to grayscale
        grayRow :: Grayscale width (height - 1) depth restOfImage // Append the converted row to the result of the recursive call.


  
  let rec Threshold (width:int) 
                    (height:int)
                    (depth:int)
                    (image:(int*int*int) list list)
                    (threshold:int) = 
    
    image


  
  let rec FlipHorizontal (width:int)
                         (height:int)
                         (depth:int)
                         (image:(int*int*int) list list) = 
    image


  
  let rec EdgeDetect (width:int)
               (height:int)
               (depth:int)
               (image:(int*int*int) list list)
               (threshold:int) = 
    image


  let rec RotateRight90 (width:int)
                        (height:int)
                        (depth:int)
                        (image:(int*int*int) list list) = 
    image


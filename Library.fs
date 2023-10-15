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
      let depthCalculator d = 
        if d > threshold then depth
        else 0

      let thresholdPixel (r, g, b) = 
        ((depthCalculator r, depthCalculator g, depthCalculator b))
    
      match image with
      | [] -> [] // If the image list is empty, return an empty list.
      | row :: restOfImage ->
          let thresholdedRow = row |> List.map thresholdPixel
          thresholdedRow :: Threshold width (height - 1) depth restOfImage threshold


  
  let rec FlipHorizontal (width:int)
                         (height:int)
                         (depth:int)
                         (image:(int*int*int) list list) = 
    image |> List.map List.rev


  
  let rec EdgeDetect (width:int)
               (height:int)
               (depth:int)
               (image:(int*int*int) list list)
               (threshold:int) = 
     // Calculate the color difference between two pixels
    let pixelDifference (r1, g1, b1) (r2, g2, b2) =
        let dr = float (r1 - r2)
        let dg = float (g1 - g2)
        let db = float (b1 - b2)
        sqrt (dr * dr + dg * dg + db * db)
    // Check if a pixel is an edge
    let isEdge pixel rightPixel bottomPixel =
        let rightDiff = pixelDifference pixel rightPixel
        let bottomDiff = pixelDifference pixel bottomPixel
        rightDiff > float threshold || bottomDiff > float threshold


    let processRow row nextRow =
      // Drop the last pixel from both rows before zipping
      let rowWithoutLast = List.init (List.length row - 1) (fun i -> List.item i row)
      let nextRowWithoutLast = List.init (List.length nextRow - 1) (fun i -> List.item i nextRow)

      // Zip the row with its next row to get the bottom pixel
      // Then zip the result with the tail of the row to get the right pixel
      // This effectively pairs each pixel with its right and bottom neighbors
      List.zip3 rowWithoutLast (List.tail row) nextRowWithoutLast
      |> List.map (fun (pixel, rightPixel, bottomPixel) ->
          if isEdge pixel rightPixel bottomPixel then
              (0, 0, 0) // Black pixel
          else
              (255, 255, 255) // White pixel
      )

    match image with
    | [] | [_] -> []  // If the image list is empty or has only one row, return an empty list.
    | row :: nextRow :: restOfImage -> 
        processRow row nextRow :: EdgeDetect width (height - 1) depth (nextRow :: restOfImage) threshold


  let rec RotateRight90 (width:int)
                        (height:int)
                        (depth:int)
                        (image:(int*int*int) list list) = 
    // Extract a column as a list given an index 'col'
    let columnAsRow n image =
      image |> List.map (fun row -> List.item n row) |> List.rev
    let rec helper colIndex =
        if colIndex >= width then []
        else
            let newRow = columnAsRow colIndex image
            newRow :: helper (colIndex + 1)

    helper 0



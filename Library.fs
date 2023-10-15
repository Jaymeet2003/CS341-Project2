// Work on your solution here

// Do not change the API (module/namespace/function signatures)

// Refer to the PDF for more instructions on how to implement
// each function

// As of now, each function simply takes an image as input
// and returns it as it is

//
// Project Name : Project 02 : Image processing in F# 
// Project Description : The 'Library.fs' offers essential image manipulations, including grayscale conversion, thresholding, horizontal flipping, edge detection, and 90-degree rotation. These foundational tools enable basic image transformations, paving the way for advanced visual tasks.
// Name : Jimmy Patel
// Netid :jpate289
// Date : 10/15/2023
//
// ImageLibrary namespace contains operations related to image processing
namespace ImageLibrary

// Operations module contains various functions to manipulate and process images
module Operations =

    // Convert an image to grayscale
    let rec Grayscale (width:int) 
                      (height:int) 
                      (depth:int) 
                      (image:(int*int*int) list list) = 
        // Function to calculate the grayscale value for a single pixel
        let grayscalePixel (r, g, b: int): (int * int * int) =
            let gray = int ((float r * 0.299) + (float g * 0.587) + (float b * 0.114))
            (gray, gray, gray)

        match image with
        | [] -> []  // If the image list is empty, return an empty list
        | row :: restOfImage -> 
            let grayRow = row |> List.map grayscalePixel // Convert the current row to grayscale
            grayRow :: Grayscale width (height - 1) depth restOfImage // Process the rest of the image recursively
  
    // Apply a threshold to an image
    let rec Threshold (width:int) 
                      (height:int)
                      (depth:int)
                      (image:(int*int*int) list list)
                      (threshold:int) = 

        // Function to adjust pixel depth based on threshold
        let depthCalculator d = 
            if d > threshold then depth
            else 0

        // Convert a pixel based on threshold
        let thresholdPixel (r, g, b) = 
            (depthCalculator r, depthCalculator g, depthCalculator b)

        match image with
        | [] -> [] // If the image list is empty, return an empty list
        | row :: restOfImage ->
            let thresholdedRow = row |> List.map thresholdPixel // Apply threshold to the current row
            thresholdedRow :: Threshold width (height - 1) depth restOfImage threshold
  
    // Flip an image horizontally
    let rec FlipHorizontal (width:int)
                           (height:int)
                           (depth:int)
                           (image:(int*int*int) list list) = 
        image |> List.map List.rev // Reverse each row of the image
  
    // Detect edges in an image
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

        // Check if a pixel is at the edge
        let isEdge pixel rightPixel bottomPixel =
            let rightDiff = pixelDifference pixel rightPixel
            let bottomDiff = pixelDifference pixel bottomPixel
            rightDiff > float threshold || bottomDiff > float threshold

        // Process a row and identify edges
        let processRow row nextRow =
            // Drop the last pixel from both rows before zipping
            let rowWithoutLast = List.init (List.length row - 1) (fun i -> List.item i row)
            let nextRowWithoutLast = List.init (List.length nextRow - 1) (fun i -> List.item i nextRow)

            // Pair each pixel with its right and bottom neighbors
            List.zip3 rowWithoutLast (List.tail row) nextRowWithoutLast
            |> List.map (fun (pixel, rightPixel, bottomPixel) ->
                if isEdge pixel rightPixel bottomPixel then
                    (0, 0, 0) // Edge detected: Black pixel
                else
                    (255, 255, 255) // No edge: White pixel
            )

        match image with
        | [] | [_] -> []  // If the image list is empty or has only one row, return an empty list
        | row :: nextRow :: restOfImage -> 
            processRow row nextRow :: EdgeDetect width (height - 1) depth (nextRow :: restOfImage) threshold

    // Rotate an image 90 degrees to the right
    let rec RotateRight90 (width:int)
                          (height:int)
                          (depth:int)
                          (image:(int*int*int) list list) = 

        // Extract a column from the image and treat it as a row
        let columnAsRow n image =
            image |> List.map (fun row -> List.item n row) |> List.rev

        // Helper function to process each column
        let rec helper colIndex =
            if colIndex >= width then []
            else
                let newRow = columnAsRow colIndex image
                newRow :: helper (colIndex + 1)

        helper 0

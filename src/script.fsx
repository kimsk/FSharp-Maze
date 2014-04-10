open System
open System.Drawing
open System.Windows.Forms

type Block = Start | Finish | Path | Wall

let mazeSize = 12
let blockSize = 20
let width = (mazeSize * (blockSize + 2))
let height = (mazeSize * (blockSize + 2))
let getMaze = Array2D.init mazeSize mazeSize
let maze = getMaze (fun _ _ -> Path)

let show (maze:Block[,]) =
    let form = new Form(Width=width, Height=height)
    let box = new PictureBox(BackColor = Color.White, Dock = DockStyle.Fill)
    let image = new Bitmap(width, height)
    let graphics = Graphics.FromImage(image)
    let brush = new SolidBrush(Color.FromArgb(0, 0, 128))
    box.Image <- image
    form.Controls.Add(box) 

    let rowsNum = (Array2D.length1 maze) - 1
    let colsNum = (Array2D.length2 maze) - 1
    for r in 0..rowsNum do
        for c in 0..colsNum do
            match maze.[r,c] with
            | Wall -> graphics.FillRectangle(brush, 0 + (r*blockSize), 0 + (c*blockSize), blockSize, blockSize)
            | _ -> ()
    form.ShowDialog()

let rnd = new Random()

let rec getRandom min max (nums:int seq) =
    let rndNum = rnd.Next(min, max)
    match nums |> Seq.tryFind ((=)rndNum) with
    | None -> rndNum
    | _ -> getRandom min max nums

let getRandomMaze:seq<int>->int = getRandom 0 (mazeSize-1)

getRandomMaze [1;2;3;4]

let buildWall (maze:Block[,]) =
    let wallR = getRandomMaze []
    let wallC = getRandomMaze []
    let holeR = getRandomMaze [wallR]
    let holeC = getRandomMaze [wallC]
    getMaze (fun r c -> 
                match r,c with
                | r,c when r = wallR && c <> holeC -> Wall
                | r,c when c = wallC && r <> holeR -> Wall
                | _ -> maze.[r,c])

maze |> buildWall |> show
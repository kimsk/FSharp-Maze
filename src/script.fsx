open System
open System.Drawing
open System.Windows.Forms

type Block = Start | Finish | Path | Wall

let mazeSize = 12
let blockSize = 20
let getMaze = Array2D.init mazeSize mazeSize
let maze = getMaze (fun _ _ -> Path)

let show title (maze:Block[,]) =
    let height = ((Array2D.length1 maze) + 2) * blockSize
    let width = ((Array2D.length2 maze) + 1) * blockSize
    
    let form = new Form(Text = title, Width=width, Height=height)
    let box = new PictureBox(BackColor = Color.Magenta, Dock = DockStyle.Fill)
    let image = new Bitmap(width, height)
    let graphics = Graphics.FromImage(image)
    let brush = new SolidBrush(Color.FromArgb(0, 0, 128))
    let pen = new Pen(Color.White)
    box.Image <- image
    form.Controls.Add(box) 
        

    let rowsNum = (Array2D.length1 maze) - 1
    let colsNum = (Array2D.length2 maze) - 1
    for r in 0..rowsNum do
        for c in 0..colsNum do
            match maze.[r,c] with
            | Wall -> 
                let rect = new Rectangle(0 + (r*blockSize), 0 + (c*blockSize), blockSize, blockSize)
                graphics.FillRectangle(brush, rect)
                graphics.DrawRectangle(pen, rect)
            | _ -> ()
    form.Show()

let rnd = new Random()

let rec getRandom min max (nums:int seq) =
    let rndNum = rnd.Next(min, max)
    match nums |> Seq.tryFind ((=)rndNum) with
    | None -> rndNum
    | _ -> getRandom min max nums

let getRandomMaze:seq<int>->int = getRandom 0 (mazeSize-1)

let divideMaze wallR wallC height width =    
    let topLeft = Array2D.init (wallR - 1) (wallC - 1) (fun r c -> Path)
    let topRight = Array2D.init (wallR - 1) (width - wallC - 1) (fun r c -> Path)
    let bottomLeft = Array2D.init (height - wallR - 1) (wallC) (fun r c -> Path)
    let bottomRight = Array2D.init (height - wallR - 1) (width - wallC - 1) (fun r c -> Path)
    topLeft |> show "top-left"
    topRight |> show "top-right"
    bottomLeft |> show "bottom-left"
    bottomRight |> show "bottom-right"

let buildWall (maze:Block[,]) =
    let width = Array2D.length2 maze
    let height = Array2D.length1 maze

    let wallR = rnd.Next(0, height)
    let wallC = rnd.Next(0, width)
    let holeR = getRandomMaze [wallR]
    let holeC = getRandomMaze [wallC]

    divideMaze wallR wallC height width

    getMaze (fun r c -> 
                match r,c with
                | r,c when r = wallR && c <> holeC -> Wall
                | r,c when c = wallC && r <> holeR -> Wall
                | _ -> maze.[r,c])

maze |> buildWall |> show "maze"

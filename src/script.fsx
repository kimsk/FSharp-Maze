open System
open System.Drawing
open System.Windows.Forms

type Block = Start | Finish | Path | Wall

let mazeSize = 12
let blockSize = 20
let getMaze = Array2D.init

let show (maze:Block[,]) =
    let height = (Array2D.length1 maze) * blockSize
    let width = (Array2D.length2 maze) * blockSize    
//    let form = new Form(Text = title, Width = (width + 16), Height = (height + 39), ControlBox = false, ShowIcon = false)        
    let form = new Form(FormBorderStyle = FormBorderStyle.None, Width = width, Height = height)
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
                let rect = new Rectangle(c*blockSize, r*blockSize, blockSize, blockSize)
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
    let topLeft = Array2D.init (wallR) (wallC) (fun r c -> Path)
    let topRight = Array2D.init (wallR) (width - wallC) (fun r c -> Path)
    let bottomLeft = Array2D.init (height - wallR - 1) (wallC) (fun r c -> Path)
    let bottomRight = Array2D.init (height - wallR - 1) (width - wallC) (fun r c -> Path)    
    topLeft |> printfn "%A"
    topRight |> printfn "%A"
    bottomLeft |> printfn "%A"
    bottomRight |> printfn "%A"

let buildWall (maze:Block[,]) =
    let height = Array2D.length1 maze
    let width = Array2D.length2 maze

    let wallR = rnd.Next(0, height)
    let wallC = rnd.Next(0, width)
    let holeR = getRandomMaze [wallR]
    let holeC = getRandomMaze [wallC]
    printfn "%d %d %d %d" wallR wallC height width
    divideMaze wallR wallC height width
    
    getMaze height width (fun r c -> 
                match r, c with
                | _ when r = wallR && c <> holeC -> Wall
                | _ when c = wallC && r <> holeR -> Wall
                | _ -> maze.[r,c])

getMaze mazeSize mazeSize (fun _ _ -> Path) |> buildWall |> show

getMaze 10 20 (fun _ _ -> Path) |> (fun m -> printfn "%d %d" (Array2D.length1 m) (Array2D.length2 m))

getMaze 12 12 (fun r c -> (r,c)) |> printfn "%A"

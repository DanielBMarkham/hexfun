type HexDisplayOrientation = PointyTop | FlatTop
let vertexIndices=[0..5]
let vertexAngles(displayOrientation:HexDisplayOrientation)=
  match displayOrientation 
    with 
      |PointyTop -> [30.0;90.0;150.0;210.0;270.0;330.0]
      |FlatTop -> [0.0;60.0;120.0;180.0;240.0;300.0]
let sidesAngles(displayOrientation:HexDisplayOrientation)=
  match displayOrientation 
    with 
      |PointyTop -> [0.0;60.0;120.0;180.0;240.0;300.0]
      |FlatTop ->  [30.0;90.0;150.0;210.0;270.0;330.0]
let getWidthHeightPair displayOrientation size = 
  match displayOrientation 
    with 
    |PointyTop -> (System.Math.Sqrt(3) * size ,2.0*size)
    |FlatTop -> (2.0*size, System.Math.Sqrt(3)*size)
let getHexagonCornerFromOrigin (displayOrientation:HexDisplayOrientation) hexSize cornerIndex =
  let wrapAroundIndex= cornerIndex%6 // NEVER ERROR
  let angleToCorner = (vertexAngles displayOrientation)|>List.item wrapAroundIndex
  let pointX=hexSize*System.Math.Cos(angleToCorner*System.Math.PI/180.0) // Remember to convert to rads
  let pointY=hexSize*System.Math.Sin(angleToCorner*System.Math.PI/180.0)
  (pointX,pointY)

// type HexagonCoordinateSystemType = OffsetCoordinates | CubeCoordinates | AxialCoordinates | DoubledCoordinates
type HexagonCoordinateSystemType = 
  OffsetCoordinatesOddR
  | OffsetCoordinatesEvenR
  | OffsetCoordinatesOddQ
  | OffsetCoordinatesEvenQ
  | CubeCoordinates 
  | AxialCoordinates 
  | DoubledCoordinatesDoubledWidth
  | DoubledCoordinatesDoubledHeight


type HexagonalCoordinates = 
  | OffsetCoordinatesOddR of int*int
  | OffsetCoordinatesEvenR of int*int
  | OffsetCoordinatesOddQ of int*int
  | OffsetCoordinatesEvenQ of int*int
  | CubeCoordiantes of int*int*int
  | AxialCoordinates of int*int
  | DoubledCoordinatesDoubledWidth of int*int 
  | DoubledCoordinatesDoubledHeight of int*int 

let getSomeAnswer (coord:HexagonalCoordinates) = 
  match coord with 
    | OffsetCoordinatesOddR(q,r)->7
    | OffsetCoordinatesEvenR(q,r)->7
    | OffsetCoordinatesOddQ(q,r)->7
    | OffsetCoordinatesEvenQ(q,r)->7
    | CubeCoordiantes(q,r,s)->3
    | AxialCoordinates(q,r)->9
    | DoubledCoordinatesDoubledWidth(q,r)->1234
    | DoubledCoordinatesDoubledHeight(q,r)->1234

let foo =  DoubledCoordinatesDoubledWidth(9,0)

printfn "Hello from F#"

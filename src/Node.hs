module Node where

import Agda.Syntax.Concrete
import Agda.Syntax.Concrete.Pretty

data Node
    = Text String
    | SpaceOrLine
    | Line
    | Indent [Node]
    | Nodes [Node]

node :: Declaration -> Node
node (TypeSig _ _ name expr) =
    Nodes
        [ Text $ nameToRawName name
        , SpaceOrLine
        , Text ":"
        , SpaceOrLine
        , Text $ prettyShow expr
        , Line
        ]
node (FieldSig _ _ name expr) =
    Nodes
        [ Text $ nameToRawName name
        , SpaceOrLine
        , Text ":"
        , SpaceOrLine
        , Text $ prettyShow expr
        , Line
        ]
node (Generalize _ expr) =
    Nodes
        [ Text "generalize"
        , SpaceOrLine
        , Text $ prettyShow expr
        , Line
        ]
node (FunClause lhs rhs _ _) =
    Nodes
        [ Text $ prettyShow lhs
        , SpaceOrLine
        , Text $ prettyShow rhs
        , Line
        ]
node (Postulate _ decls) =
    Nodes $
        [ Text "postulate"
        , SpaceOrLine
        , Text ":"
        , SpaceOrLine
        ]
            ++ map node decls
node (Open _ qname _) =
    Nodes
        [ Text "open"
        , SpaceOrLine
        , Text $ prettyShow qname
        , Line
        ]
node (Import _ qname _ _ _) =
    Nodes
        [ Text "import"
        , SpaceOrLine
        , Text $ prettyShow qname
        , Line
        ]
node (Module _ _ qname _ decls) =
    Nodes $
        [ Text "module"
        , SpaceOrLine
        , Text $ prettyShow qname
        , SpaceOrLine
        , Text "where"
        , Line
        , Line
        ]
            ++ map node decls
node (Data _ _ name _ expr decls) =
    Nodes
        [ Text "data"
        , SpaceOrLine
        , Text $ prettyShow name
        , SpaceOrLine
        , Text ":"
        , SpaceOrLine
        , Text $ prettyShow expr
        , SpaceOrLine
        , Text "where"
        , Line
        , Indent $ map node decls
        ]
node _ = Text ""

width :: Node -> Int
width (Text s) = length s
width SpaceOrLine = 1
width Line = 1
width (Nodes ns) = sum $ map width ns
width _ = 0

module ExpressionType

    type expression =
        | Expr_Op of char * expression * expression
        | Expr_Sym of string
        | Expr_Lbl of string
        | Expr_Dec of int16
        | Expr_Oct of int16
        | Expr_DChar of char * char
        | Expr_SChar of char
        | Expr_Group of expression


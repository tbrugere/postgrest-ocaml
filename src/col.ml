open Ty

type ('table, 'a) col = {
    ty: 'a ty;
    name: string;
}

type ('sourcetable, 'desttable) relationship = {
    name: string;
}

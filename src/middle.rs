use strum::AsRefStr;

#[derive(Debug, Clone, Copy)]
pub struct IRType {
    pub(crate) is_list: bool,
    pub(crate) primitive: IRPrimitiveType,
}
#[derive(Debug, Clone, Copy, AsRefStr)]
pub enum IRPrimitiveType {
    Number,
    Point2,
    Point3,
    Polygon,
    Bool,
    Never,
}
#[derive(Debug, Clone, Copy)]
pub enum Comparison {
    Eq,
    GreaterEq,
    Greater,
    LessEq,
    Less,
}

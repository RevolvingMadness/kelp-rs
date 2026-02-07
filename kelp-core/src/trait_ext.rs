pub trait OptionIterExt {
    fn all_some(self) -> Option<()>;
}

impl<I> OptionIterExt for I
where
    I: Iterator<Item = Option<()>>,
{
    fn all_some(self) -> Option<()> {
        let mut failed = false;

        for item in self {
            failed |= item.is_none();
        }

        (!failed).then_some(())
    }
}

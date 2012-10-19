## UnGADTagger

This package encapsulates abstraction-from-gadt-tag workflow by utilizing existentials, rank2-types and kind polymorphism.

## Example

    {-# LANGUAGE GADTs, DataKinds, KindSignatures, LambdaCase #-}
    
    import Data.GADT.Untagged

    data Tag = A | B

    data Gadt :: Tag -> * where
      ConA :: String   -> Gadt A
      ConB :: [Gadt A] -> Gadt B
      ConC :: Integer  -> Gadt B

    foo :: something -> [Untagged Gadt] -> String
    foo _ = quux
      where
        quux  = concat . map baz
        baz x = match x $ \case
          ConA s  -> s
          ConB xs -> quux (map untag xs)
          ConC i  -> show i

    test = foo "some cfg for e.g." (xs ++ ys)
      where
        xs = map untag [ConA "A1..", ConA "A2.."]
        ys = map untag [ConB [ConA "A3.."], ConC 456]

    -- > "A1..A2..A3..456"
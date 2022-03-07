{-# language DataKinds, PolyKinds, FunctionalDependencies #-}

module ProAbstract.Pro where

import ProAbstract.Types

type family Pro seq ann (a :: k) :: Type
type instance Pro seq ann Block = Block ann
type instance Pro seq ann Blocks = Blocks ann
type instance Pro seq ann BlockTag = BlockTag ann
type instance Pro seq ann BlockTagContent = BlockTagContent ann
type instance Pro seq ann Document = Document ann
type instance Pro seq ann Fragment = Fragment ann
type instance Pro seq ann Inline = Inline ann
type instance Pro seq ann Line = Line ann
type instance Pro seq ann Lines = Lines ann
type instance Pro seq ann Paragraph = Paragraph ann
type instance Pro seq ann PlainBlock = PlainBlock ann
type instance Pro seq ann Tag = Tag ann
type instance Pro seq ann TaggedBlocks = TaggedBlocks ann
type instance Pro seq ann TaggedLines = TaggedLines ann
type instance Pro seq ann TaggedPlainBlock = TaggedPlainBlock ann

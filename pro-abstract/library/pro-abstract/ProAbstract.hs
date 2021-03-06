{- |
Copyright: © 2020 James Alexander Feldman-Crough
           © 2022 Mission Valley Software LLC
License: MPL-2.0


=== Document

/Document/ is the root of the tree.

- 'Document' = ('metadata' : 'Metadata') × ('content' : 'Blocks')


=== Tagged nodes

- 'Tag' = ('name' : 'Text') × ('metadata' : 'Metadata') × ('annotation' : 'Annotation')

/Tagged/ nodes are branch points within a document.

- 'TaggedBlocks'     = ('tag' : 'Tag') × ('content' : 'Blocks')
- 'TaggedPlainBlock' = ('tag' : 'Tag') × ('content' : 'PlainBlock')
- 'TaggedLines'      = ('tag' : 'Tag') × ('content' : 'Lines')

Optics:

- 'tag' targets the tag at a node; it is either a lens or an affine traversal, depending on the node type.
- The 'allTags' traversal targets every tag at and below a node.
- The 'allBlockTags' and 'allInlineTags' also target tags at or below a node, but are limited to tags at the block or inline level, respectively.
- Since a tag has metadata, all of the optics for targeting parts of metadata (e.g. 'atSetting' and 'hasProperty') are also available on 'Tag' and on @Tagged...@ nodes.

Operations for altering tags and/or removing tagged nodes in bulk:

  - Pure: 'mapMaybeTags', 'mapMaybeBlockTags', 'mapMaybeInlineTags'
  - Monadic: 'witherTags', 'witherBlockTags', 'witherInlineTags'

Example: @'mapMaybeTags' (preview (filtered \\x -> view 'name' x /= "comment"))@ is an endofunction that removes all nodes having a tag name of "comment".


=== The Block level

/Block-level/ content is everything that appears below the document level and above the paragraph level.

- 'Block' = ('fork' : 'TaggedBlocks') ∪ ('plain' : 'TaggedPlainBlock') ∪ ('bare' : 'Paragraph')

There are three kinds of block:

- /Fork/ blocks contain more blocks.

    * 'TaggedBlocks' = ('tag' : 'Tag') × ('contents' : 'Block')

- /Plain/ blocks contain text.

    * 'TaggedPlainBlock' = ('tag' : 'Tag') × ('content' : 'PlainBlock')
    * 'PlainBlock' = ('contents' : 'Fragment') × ('annotation' : 'Annotation')

- /Bare/ blocks (paragraphs) contain inline content.

    * 'Paragraph' = ('contents' : 'Line') × ('annotation' : 'Annotation')

Unenforced guideline: The 'Lines' of a 'Paragraph' should contain at least one 'Line'.


=== BlockTag

A /BlockTag/ is a non-paragraph Block.

- 'BlockTag' = ('fork' : 'TaggedBlocks') ∪ ('plain' : 'TaggedPlainBlock')

The Block type can be described in terms of BlockTag as:

- 'Block' = ('tagged' : 'BlockTag') ∪ ('bare' : 'Paragraph')

Example: @'tagged' % 'Optics.Core.filtered' (\x -> 'Optics.Core.view' 'name' x == "h1")@ is an affine fold that targets blocks with a tag name of "h1".

The BlockTag type can also be described as:

- 'BlockTag' = ('tag' :: 'Tag') × ('content' : 'BlockTagContent')
- 'BlockTagContent' = ('fork' : 'Blocks') ∪ ('plain' : 'PlainBlock')


=== The Inline level

/Inline-level/ content is everything below the paragraph level.

Inline content is grouped into lines; we specify no particular semantics of line breaks but suggest that a typical consumer of 'Lines' will fold them together with a single space character interspersed between the lines.

- 'Inline' = ('fork' or 'tagged' : 'TaggedLines') ∪ ('plain' or 'bare' : 'Fragment')

There are two kinds of inline:

- /Fork/ or /tag/ inlines contain more inlines.

    * 'TaggedLines' = ('tag' : 'Tag') × ('content' : 'Lines')
    * 'Lines' = ('contents' : 'Line')
    * 'Line' = ('content' : 'Seq' 'Inline') × ('annotation' : 'Annotation')

- /Plain/ or /bare/ inlines contain text.

Unenforced guideline: Each 'Line' should contain at least one 'Inline'.


=== Fragments

/Fragments/ are all the little scraps of text that serve as the leaves of the document tree.

- 'Fragment' = ('content' : 'Text') × ('annotation' : 'Annotation')

A fragment may appear at the inline level as a plain 'Inline', or at the block level as a line of a 'PlainBlock'.

Unenforced guidelines: Fragment text should be non-empty, and it should not contain any line break characters.


=== Metadata

Documents and tags both have /metadata/.

- 'Metadata' = ('properties' : 'Set' 'Text') × ('settings' : 'Map' 'Text' 'Text')

Optics:

- 'metadata' targets the metadata of a node; it is either a lens or an affine traversal, depending on the node type.
- 'atSetting' and 'hasProperty' target specific parts of metadata by name.
- The 'allMetadata' traversal targets every metadata at and under a node.
- 'metaMap' gives properties and settings combined into a single 'Map' 'Text' 'MetaValue'
- 'metaList' gives properties and settings combined into a list of 'MetaItem'


=== Content

The /content/ of a node is whatever is nested directly under it in the document tree. The 'content' lens is overloaded via the 'Content' type family and 'HasContent' class.

The /contents/ of a node is the type of sequence element most directly nested under it. The 'contents' lens is via the 'Contents' type family and 'HasContents' class.

List of nodes and their content types:

+-----------------------+-----------------------+-----------------------+
| __x__                 | __Content x__         | __Contents x__        |
+-----------------------+-----------------------+-----------------------+
| 'Document'            | 'Blocks'              | 'Block'               |
+-----------------------+-----------------------+-----------------------+
| 'TaggedBlocks'        | 'Blocks'              | 'Block'               |
+-----------------------+-----------------------+-----------------------+
| 'Blocks'              | —                     | 'Block'               |
+-----------------------+-----------------------+-----------------------+
| 'BlockTag'            | 'BlockTagContent'     | —                     |
+-----------------------+-----------------------+-----------------------+
| 'TaggedPlainBlock'    | 'PlainBlock'          | 'Fragment'            |
+-----------------------+-----------------------+-----------------------+
| 'PlainBlock'          | —                     | 'Fragment'            |
+-----------------------+-----------------------+-----------------------+
| 'Paragraph'           | 'Lines'               | 'Line'                |
+-----------------------+-----------------------+-----------------------+
| 'TaggedLines'         | 'Lines'               | 'Line'                |
+-----------------------+-----------------------+-----------------------+
| 'Lines'               | —                     | 'Line'                |
+-----------------------+-----------------------+-----------------------+
| 'Line'                | —                     | 'Inline'              |
+-----------------------+-----------------------+-----------------------+
| 'Fragment'            | 'Text'                | —                     |
+-----------------------+-----------------------+-----------------------+

'HasContent' and 'HasContents' have type aliases 'HasContents'' and 'HasContents'' respectively for types that only support simple optics.


=== Tagged or bare

At both the block and inline level, a node is either /tagged/ or /bare/, a fact manifested by the 'taggedOrBare' isomorphism of the 'IsTaggedOrBare' class.

The 'tagged' prism is overloaded via the 'TaggedType' type family, and the 'bare' prism is overloaded via the 'BareType' family.

List of nodes and their tagged/bare types:

+-----------------------+-----------------------+-----------------------+
| __x__                 | __TaggedType x__      | __BareType x__        |
+-----------------------+-----------------------+-----------------------+
| 'Block'               | 'BlockTag'            | 'Paragraph'           |
+-----------------------+-----------------------+-----------------------+
| 'Inline'              | 'TaggedLines'         | 'Fragment'            |
+-----------------------+-----------------------+-----------------------+


=== Fork or plain

At both the block and inline level, a node may be a /fork/ that contains a sequence of more content at the same level, and a node may be /plain/ node that just contains text.

The 'fork' prism is overloaded via the 'Fork' type family and 'CanFork' class.

The 'plain' prism is overloaded via the 'Plain' type family and 'CanBePlain' class.

List of nodes and their fork/plain types:

+-----------------------+--------------------+-----------------------+
| __x__                 | __Fork x__         | __Plain x__           |
+-----------------------+--------------------+-----------------------+
| 'Block'               | 'TaggedBlocks'     | 'TaggedPlainBlock'    |
+-----------------------+--------------------+-----------------------+
| 'BlockTag'            | 'TaggedBlocks'     | 'TaggedPlainBlock'    |
+-----------------------+--------------------+-----------------------+
| 'BlockTagContent'     | 'Blocks'           | 'PlainBlock'          |
+-----------------------+--------------------+-----------------------+
| 'Inline'              | 'TaggedLines'      | 'Fragment'            |
+-----------------------+--------------------+-----------------------+

('Block' also has a third prism, 'bare', which is neither fork nor plain.)


=== Annotation

An /annotation/ is attached to every 'Tag', 'PlainBlock', 'Paragraph', 'Line', and 'Fragment'. The 'Annotation' type may be anything, but its original purpose was to map each component of a document to the location where it appeared in a text file.

Optics:

- The 'annotation' lens targets a single annotation at a node
- The 'allAnnotations' traversal targets every annotation at and under a node.


=== Modules

The ProAbstract module provides everything in the pro-abstract package. There are also some smaller collections of reëxports.

Organized by topic:

- "ProAbstract.Annotation"
- "ProAbstract.Content"
- "ProAbstract.Metadata"
- "ProAbstract.Structure"
- "ProAbstract.Tag"
- "ProAbstract.Tagless"

Organized by kind of thing:

- "ProAbstract.Types" — The essential types, exported abstractly. Includes type families and data families.
- "ProAbstract.Optics" — Lenses, prisms, traversals, etc. Many of these are class methods, but are exported here without their classes.
- "ProAbstract.Datatypes" — Datatypes including with their constructors and record fields
- "ProAbstract.Families" — Type families and data families.
- "ProAbstract.Classes" — Typeclasses, mostly for the polymorphic optics.

-}
module ProAbstract
    (
    {- * Document -} Document (..),

    {- * Blocks -} Block (..), Blocks (..), TaggedBlocks (..),
    {- ** Paragraphs -} Paragraph (..), HasManyParagraphs (..),
    {- ** Tag blocks -} BlockTag (..), BlockTagContent (..),

    {- * Lines -} Inline (..), Line (..), Lines (..), TaggedLines (..),
    {- ** Plain text -} Fragment (..), PlainBlock (..), TaggedPlainBlock (..),
            HasManyPlainInlines (..), HasManyPlainBlocks (..),

    {- * Tags -} Tag (..), name, HasTag (..),
    {- ** Traversal -} HasManyTags (..), HasManyBlockTags (..),
    {- ** Withering -} HasWitherableTags (..),
            HasWitherableInlineTags (..), HasWitherableBlockTags (..),

    {- * Prisms -}
    {- ** Plain/fork -} Plain, CanBePlain (..), Fork, CanFork (..),
    {- ** Tagged/bare -} tagged, bare, IsTaggedOrBare (..), TaggedOrBare (..),

    {- * Metadata -} Metadata (..), HasMetadata (..), HasManyMetadata (..),
    {- ** Properties -} properties, hasProperty,
    {- ** Settings -} settings, atSetting,
    {- ** Item list -} MetaItem (..), metaList,
    {- ** Value map -} MetaValue (..), metaMap,

    {- * Content -} Content, HasContent (..), HasContent',
    {- ** Contents -} Contents, HasContents (..), HasContents',

    {- * Annotation -} Annotation, HasAnnotation (..), HasAnnotation',
            HasManyAnnotations (..),

    {- * Tagless content -} CanBeTagless (..), CanHaveTaglessContent (..),
            KindOfText (..)

    ) where

import ProAbstract.Classes
import ProAbstract.Datatypes
import ProAbstract.Optics
import ProAbstract.Types

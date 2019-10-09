## TASK
Having a simple immutable data structure that looks like binary tree

`case class Node[T](value: T, left: Option[Node[T]], right: Option[Node[T]])

case class Tree[T](root: Node[T])`


Write a function that will insert nodes in order from left to right via horizontal depth of the tree

example

having tree

`//                   (  1  )

//             /                 \

//        (2-1)                    (2-2)

//       /     \                 /       \

(3-1-1)    (3-1-2)     (3-2-1)         (3-2-2)

`

So insertion order results in

`1

2-1


2-2

3-1-1

3-1-2

3-2-1

3-2-2`

next will be left node of 3-1-1

then right node of 3-1-1

then left node of 3-1-2

....

#### note

we don't care about real binary tree, and values can be any type

we just care about correct insertion(see above)

and the way how you will implement copy for a new immutable tree

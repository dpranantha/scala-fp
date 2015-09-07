package list

/**
 * @author dpranantha
 */
sealed trait SimpleTree[+A] 
case class Leaf[A](value : A) extends SimpleTree[A] 
case class Branch[A](left : SimpleTree[A], right: SimpleTree[A]) extends SimpleTree[A]

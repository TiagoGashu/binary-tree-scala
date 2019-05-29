package com.gashu.bst

/**
  * @author tiagogashu in 28/05/19
  **/
trait Tree {
  def parent: Option[Tree]
  def value: Int
  def children: Array[Tree]
  // prints the path to this tree node
  override def toString: String = s"$parent\nValue: $value"
}

object BinarySearchTree {

  def createRoot(value: Int) = new BinarySearchTree(value, None, None, None)

  case class RecInsert(newValue: Int, currentNode: BinarySearchTree, leftChild: Option[BinarySearchTree], rightChild: Option[BinarySearchTree])
  case class RecSearch(value: Int, currentNode: Option[BinarySearchTree])
  case class RecRemove(value: Int, parent: Option[BinarySearchTree], currentNode: BinarySearchTree, leftChild: Option[BinarySearchTree], rightChild: Option[BinarySearchTree])
}

class BinarySearchTree(

val value: Int,
var parent: Option[BinarySearchTree],
var leftChild: Option[BinarySearchTree],
var rightChild: Option[BinarySearchTree]

) extends Tree {
  import com.gashu.bst.BinarySearchTree.{RecInsert, RecRemove, RecSearch}

  def children = Array[Tree](leftChild.get, rightChild.get)

  def insert(newValue: Int): Unit = {

    def recInsert(r: RecInsert): Unit = r match {
      case RecInsert(v, currentNode, _, _) if v == currentNode.value => Unit
      case RecInsert(v, currentNode, None, _) if v < currentNode.value =>
        currentNode.leftChild = Some(new BinarySearchTree(v, Some(currentNode), None, None))
      case RecInsert(v, currentNode, _, None) if v > currentNode.value  =>
        currentNode.rightChild = Some(new BinarySearchTree(v, Some(currentNode), None, None))
      // recursive call to lefChild
      case RecInsert(v, currentNode, lc, _)
        if v != currentNode.value && v < currentNode.value =>
        recInsert(RecInsert(v, lc.get, lc.get.leftChild, lc.get.rightChild))
      // recursive call to rightChild
      case RecInsert(v, currentNode, _, rc)
        if v != currentNode.value && v > currentNode.value =>
        recInsert(RecInsert(v, rc.get, rc.get.leftChild, rc.get.rightChild))
    }

    recInsert(RecInsert(newValue, this, leftChild, rightChild))
  }

  def size(): Int =
    1 +
      (if (leftChild.isDefined) leftChild.get.size() else 0) +
      (if (rightChild.isDefined) rightChild.get.size() else 0)

  // pre-order search
  def search(value: Int): Option[BinarySearchTree] = {

    def recSearch(r: RecSearch): Option[BinarySearchTree] = r match {
      case RecSearch(v, currentNode) if currentNode.isEmpty => None
      case RecSearch(v, currentNode) if v == currentNode.get.value => currentNode
      case RecSearch(v, currentNode) if v < currentNode.get.value =>
        recSearch(RecSearch(v, currentNode.get.leftChild))
      case RecSearch(v, currentNode) if v > currentNode.get.value =>
        recSearch(RecSearch(value, currentNode.get.rightChild))
    }

    recSearch(RecSearch(value, Some(this)))
  }

  def remove(value: Int): Boolean = {

    def toRecRemove(value: Int, node: BinarySearchTree) = RecRemove(value, node.parent, node, node.leftChild, node.rightChild)

    def recRemove(r: RecRemove): Boolean = r match {
      // I'm removing the root!!
      case RecRemove(v, None, node, _, _) if node.value == v => false

      case RecRemove(v, p, node, None, None) if node.value == v => p.get rearrange (node, None)
      case RecRemove(v, p, node, lc, None) if node.value == v => p.get rearrange(node, lc)
      case RecRemove(v, p, node, None, rc) if node.value == v => p.get rearrange(node, rc)

      // worst case scenario
      case RecRemove(v, p, node, _, _) if node.value == v => p.get rearrange node

      // cases of recursive call
      case RecRemove(v, p, node, lc, _) if lc.isDefined && v < node.value =>
        recRemove(RecRemove(v, Some(node), lc.get, lc.get.leftChild, lc.get.rightChild))
      case RecRemove(v, p, node, _, rc) if rc.isDefined && v > node.value =>
        recRemove(RecRemove(v, Some(node), rc.get, rc.get.leftChild, rc.get.rightChild))

      // didn't find node to remove
      case _ => false
    }

    recRemove(toRecRemove(value, this))
  }

  // aux

  // grandson becomes son of this node
  private def rearrange(son: BinarySearchTree, grandson: Option[BinarySearchTree]): Boolean = {
    println("My son only has one son")
    val isLeftChild = this.leftChild.isDefined && (this.leftChild.get eq son)
    val isRightChild = this.rightChild.isDefined && (this.rightChild.get eq son)
    if(!isLeftChild && !isRightChild) return false

    val newSon: Option[BinarySearchTree] = if(grandson.isDefined) grandson else None

    if (isLeftChild) this.leftChild = newSon
    else this.rightChild = newSon
    true
  }

  // searches in-order for a substitute node
  private def rearrange(son: BinarySearchTree): Boolean = {
    println("My son has two sons")
    // TODO
    true
  }

}

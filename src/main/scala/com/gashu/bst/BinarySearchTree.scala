package com.gashu.bst

import com.gashu.bst.BinarySearchTree.RecSearchLeaf

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
  case class RecSearchLeaf(currentNode: BinarySearchTree)
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
      case RecInsert(v, currentNode, _, _)
        if v == currentNode.value => Unit
      case RecInsert(v, currentNode, None, _)
        if v < currentNode.value =>
        currentNode.leftChild = Some(new BinarySearchTree(v, Some(currentNode), None, None))
      case RecInsert(v, currentNode, _, None)
        if v > currentNode.value  =>
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

  def remove(value: Int): BinarySearchTree = {

    def toRecRemove(value: Int, node: BinarySearchTree) = RecRemove(value, node.parent, node, node.leftChild, node.rightChild)

    def recRemove(r: RecRemove): BinarySearchTree = r match {

      case RecRemove(v, None, node, lc, rc) if node.value == v => {
        if(lc.isEmpty && rc.isEmpty) null
        else if(lc.isEmpty || rc.isEmpty){
          val child = if(lc.isEmpty) rc.get else lc.get
          child.parent = None
          child
        } else {
          val optSuccessor = searchLeaf(RecSearchLeaf(rc.get))
          if(optSuccessor.isEmpty) throw new RuntimeException("No successor found!")

          val successor = optSuccessor.get

          val sParent = successor.parent
          if(sParent.isDefined) {
            val splChild = sParent.get.leftChild
            if(splChild.get eq successor) sParent.get.leftChild = None
            else sParent.get.rightChild = None
          }
          successor.parent = None

          lc.get.parent = optSuccessor
          successor.leftChild = lc

          rc.get.parent = optSuccessor
          successor.rightChild = rc

          successor
        }
      }

      case RecRemove(v, p, node, lc, rc) if node.value == v => {
        if(lc.isEmpty || rc.isEmpty) {
          val child = if (lc.isEmpty) rc else lc
          p.get rearrange(node, child)
        } else {
          p.get rearrangeWithSuccessor node
        }
        this
      }

      // cases of recursive call
      case RecRemove(v, p, node, lc, _) if lc.isDefined && v < node.value =>
        recRemove(RecRemove(v, Some(node), lc.get, lc.get.leftChild, lc.get.rightChild))
      case RecRemove(v, p, node, _, rc) if rc.isDefined && v > node.value =>
        recRemove(RecRemove(v, Some(node), rc.get, rc.get.leftChild, rc.get.rightChild))

      // didn't find node to remove
      case _ => this
    }

    recRemove(toRecRemove(value, this))
  }

  // aux

  def searchLeaf(r: RecSearchLeaf): Option[BinarySearchTree] = r match {
    case RecSearchLeaf(node) if node.leftChild.isDefined => searchLeaf(RecSearchLeaf(node.leftChild.get))
    case RecSearchLeaf(node) if node.leftChild.isEmpty && node.rightChild.isEmpty => Some(node)
    case RecSearchLeaf(node) if node.rightChild.isDefined => searchLeaf(RecSearchLeaf(node.rightChild.get))
    case _ => Option.empty
  }

  // grandson becomes son of this node
  private def rearrange(son: BinarySearchTree, grandson: Option[BinarySearchTree]): Unit = {
    println("My son only has one son")
    val isLeftChild = this.leftChild.isDefined && (this.leftChild.get eq son)
    val isRightChild = this.rightChild.isDefined && (this.rightChild.get eq son)
    if (!isLeftChild && !isRightChild) return null

    val newSon: Option[BinarySearchTree] = if (grandson.isDefined) grandson else None

    if (newSon.isDefined) {
      newSon.get.parent = Some(this)
    }
    if (isLeftChild) this.leftChild = newSon
    else this.rightChild = newSon
  }

  // searches in-order for a substitute node
  private def rearrangeWithSuccessor(son: BinarySearchTree): Unit = {
    val optSuccessor = searchLeaf(RecSearchLeaf(son.rightChild.get))
    if(optSuccessor.isEmpty) throw new RuntimeException("No successor found!")
    val successor = optSuccessor.get

    // successor loses his current parent
    val successorParent = successor.parent
    if(successorParent.isDefined) {
      val spLChild = successorParent.get.leftChild
      if (spLChild.isDefined && (spLChild.get eq successor)) successorParent.get.leftChild = None
      else successorParent.get.rightChild = None
    }

    // successor children points to the children of son
    successor.leftChild = son.leftChild
    successor.rightChild = son.rightChild

    // successor -> this (new parent)
    successor.parent = Some(this)
    // this -> successor
    val isLeftChild = this.leftChild.isDefined && (this.leftChild.get eq son)
    if(isLeftChild) this.leftChild = optSuccessor
    else this.rightChild = optSuccessor
  }

}

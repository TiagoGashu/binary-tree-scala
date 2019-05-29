package com.gashu.bst

import org.scalatest.WordSpec

/**
  * @author tiagogashu in 28/05/19
  **/
class BinarySearchTreeTest extends WordSpec {

  "A Binary Search Tree" when {

    "with '0' as root" should {

      val bst = BinarySearchTree.createRoot(0)

      "have leftChild when inserting -10" in {
        bst insert -10
        assert(bst.leftChild.get.value == -10)
      }

      "have rightChild when inserting 10" in {
        bst insert 10
        assert(bst.rightChild.get.value == 10)
      }

      "have grandson when inserting -15" in {
        bst insert -15
        val leftChild = bst.leftChild
        assert(leftChild.get.leftChild.get.value == -15)
        println(leftChild.get.leftChild)
      }

      "have grandson when inserting -5" in {
        bst insert -5
        val leftChild = bst.leftChild
        assert(leftChild.get.rightChild.get.value == -5)
      }

      "doesn't insert a repeated node" in {
        val size = bst size

        bst.insert(-10)

        val newSize = bst size

        assert(size == newSize)
        println(s"Tree size: $size")
      }

      "returns true when searching existing values" in {
        val nodeMinusFifteen = bst search -15
        assert(nodeMinusFifteen.isDefined)
        val nodeMinusFive = bst search -5
        assert(nodeMinusFive.isDefined)
      }

      "returns false when searching for inexistent values" in {
        assert((bst search -30).isEmpty)
        assert((bst search 50).isEmpty)
      }

      "removes leaves" in {
        assert(bst remove -15)
        bst insert -15
        bst insert 15
        assert(bst remove 15)
        bst insert 15
      }

      "removes a node with only one son" in {
        assert(bst remove 10)
      }
    }

    " (full test case) " when {
      // TODO
    }

  }

}

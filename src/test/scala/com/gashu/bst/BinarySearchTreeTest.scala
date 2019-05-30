package com.gashu.bst

import org.scalatest.WordSpec

/**
  * @author tiagogashu in 28/05/19
  **/
class BinarySearchTreeTest extends WordSpec {

  "A Binary Search Tree" when {

    "with '0' as root" should {

      var bst = BinarySearchTree.createRoot(0)

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

      "returns false when searching for unexistent values" in {
        assert(bst search -30 isEmpty)
        assert(bst search 50 isEmpty)
      }

      "removes leaves" in {
        bst = bst remove -15
        assert((bst search -15).isEmpty)
        bst insert -15
        bst insert 15
        bst = bst remove 15
        assert((bst search 15).isEmpty)
        bst insert 15
      }

      "removes a node with only one son" in {
        bst = bst remove 10
        assert(bst search 10 isEmpty)
      }
    }

    "This tree should" when {

      var bst = BinarySearchTree.createRoot(50)
      bst insert 40
      bst insert 70
      bst insert 60
      bst insert 80

      "Have 60 as root after removal of 50" in {
        bst = bst remove 50
        assert(bst search 50 isEmpty)
        assert(bst.value == 60)
      }

      "Rearrange with successor when removing 70" in {
        bst insert 65
        bst remove 70
        assert(bst search 70 isEmpty)
        assert(bst search 65 isDefined)
      }

    }

  }

}

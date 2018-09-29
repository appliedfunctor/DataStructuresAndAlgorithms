package com.ajsworton.trees

import scala.annotation.tailrec

final class AvlNode(
  var key: Int,
  var left: Option[AvlNode],
  var right: Option[AvlNode],
  var height: Int
) {
  def leftRotate: AvlNode = ???

  def rightRotate: AvlNode = ???
}

object AvlNode {
  def apply(value: Int): AvlNode = new AvlNode(value, None, None, 0)
}

/*
  The Avl tree is set to be a BST with int values.
  Rule, left node holds lower, right larger, no collisions.
 */
object Avl {

  def addNode(root: AvlNode, node: AvlNode): AvlNode = {
    insertNode(root, node)
    if (!isBalanced(root)) rebalance(root)
    else root
  }

  val nodeBalanced: AvlNode => Boolean = node =>
    node.left.isEmpty ||
      node.right.isEmpty ||
      Math.abs(node.left.get.height - node.right.get.height) <= 1

  def nodeRebalance(node: AvlNode): AvlNode = {
    if (node.left.isEmpty ||
      node.right.isEmpty ||
      Math.abs(node.left.get.height - node.right.get.height) <= 1) return node
    if (node.left.get.height > node.right.get.height) node.leftRotate
    else node.rightRotate
  }

  private def isBalanced(root: AvlNode): Boolean = {

    def traverse(currentNode: AvlNode, balanced: AvlNode => Boolean): Boolean = {
      if(!balanced(currentNode)) return false

      if(currentNode.left.isEmpty)
        return currentNode.right.fold(true)(node => traverse(node, balanced))

      if(currentNode.right.isEmpty)
        return currentNode.left.fold(true)(node => traverse(node, balanced))

      traverse(currentNode.left.get, balanced) && traverse(currentNode.right.get, balanced)
    }

    traverse(root, nodeBalanced)
  }

  //todo: Implement (In progress)
  private def rebalance(root: AvlNode): AvlNode = {

    def traverse(currentNode: AvlNode, balanced: AvlNode => Boolean): AvlNode = {
      if(!balanced(currentNode)) return nodeRebalance(currentNode)

      if(currentNode.left.isEmpty)
        return currentNode.right.fold(true)(node => nodeRebalance(currentNode))

      if(currentNode.right.isEmpty)
        return currentNode.left.fold(true)(node => traverse(node, balanced))

      traverse(currentNode.left.get, balanced) && traverse(currentNode.right.get, balanced)
    }

    traverse(root, nodeBalanced)
  }

  //pre-order traversal, returning if predicate true, tree un-threaded.
  private

  @tailrec
  private def insertNode(root: AvlNode, insertion: AvlNode): Option[AvlNode] = {
    if(insertion.key == root.key) None //do nothing, collision
    else if(insertion.key < root.key) {
      root.left match {
        case None =>
          root.left = Some(insertion)
          Some(insertion)
        case Some(leftNode) => insertNode(leftNode, insertion)
      }
    } else {
      root.right match {
        case None =>
          root.right = Some(insertion)
          Some(insertion)
        case Some(rightNode) => insertNode(rightNode, insertion)
      }
    }
  }

  def find(root: AvlNode, value: Int): Option[AvlNode] = {
    if(value == root.key) Some(root)
    else if(value < root.key) root.left.flatMap(node => find(node, value))
    else root.right.flatMap(node => find(node, value))
  }

  def prepOutput(root: AvlNode): String = ???
}

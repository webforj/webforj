package com.webforj.kotlin.dsl.component.tree

import com.webforj.component.Component
import com.webforj.component.icons.IconDefinition
import com.webforj.component.tree.Tree
import com.webforj.component.tree.TreeNode
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.SingleSlotSetter
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `Tree`.
 * ```
 * ... {
 *  tree() // Empty Tree component
 *  tree {
 *    treeNode("Projects") {
 *      treeNode("Alpha") {
 *        treeNode("Planning")
 *        treeNode("Execution")
 *        treeNode("Review")
 *      }
 *      treeNode("Beta") {
 *        treeNode("Design")
 *        treeNode("Development")
 *        treeNode("Testing")
 *      }
 *    }
 *    treeNode("Departments") {
 *      treeNode("Engineering") {
 *        treeNode("Software")
 *        treeNode("Hardware")
 *      }
 *      treeNode("Marketing")
 *      treeNode("Human Resources")
 *    }
 *    expand("Departments")
 *  }
 * }
 * ```
 *
 * @param block The initialization steps for the `Tree`.
 * @return The configured `Tree`.
 * @see Tree
 * @see treeNode
 */
@WebforjDsl
fun @WebforjDsl HasComponents.tree(block: @WebforjDsl Tree.() -> Unit = {}): Tree {
  val tree = Tree()
  return init(tree, block)
}

/**
 * Creates a `TreeNode` with a [text] and/or an optional [key], adding it to the root of [Tree].
 * ```
 * ... {
 *  treeNode("text") // TreeNode component with text
 *  treeNode("text", key) // TreeNode with text and key
 * }
 * ```
 *
 * @param text The text of the `TreeNode`.
 * @param key The key of the `TreeNode`, if `null`, [text] is used.
 * @param block The initialization steps of the `TreeNode`.
 * @receiver The [Tree] to those root the `TreeNode` should be added to.
 * @return The configured `TreeNode`.
 * @see TreeNode
 * @see tree
 * @see treeNode
 */
@WebforjDsl
fun @WebforjDsl Tree.treeNode(text: String, key: Any? = null, block: @WebforjDsl TreeNode.() -> Unit = {}): TreeNode {
  val node = key?.let { TreeNode(key, text) } ?: TreeNode(text)
  node.block()
  add(node)
  return node
}

@WebforjDsl
fun <T> @WebforjDsl Tree.collapseIconSlot(block: @WebforjDsl HasComponents.() -> T) where T: Component, T: IconDefinition<*> {
  SingleSlotSetter(block).setSlot(this) {
    setCollapsedIcon(it as IconDefinition<*>)
  }
}

@WebforjDsl
fun <T> @WebforjDsl Tree.expandedIconSlot(block: @WebforjDsl HasComponents.() -> T) where T: Component, T: IconDefinition<*> {
  SingleSlotSetter(block).setSlot(this) {
    setExpandedIcon(it as IconDefinition<*>)
  }
}

@WebforjDsl
fun <T> @WebforjDsl Tree.leafIconSlot(block: @WebforjDsl HasComponents.() -> T) where T: Component, T: IconDefinition<*> {
  SingleSlotSetter(block).setSlot(this) {
    setLeafIcon(it as IconDefinition<*>)
  }
}

@WebforjDsl
fun <T> @WebforjDsl Tree.leafSelectedIconSlot(block: @WebforjDsl HasComponents.() -> T) where T: Component, T: IconDefinition<*> {
  SingleSlotSetter(block).setSlot(this) {
    setLeafSelectedIcon(it as IconDefinition<*>)
  }
}

/**
 * Creates a `TreeNode` with a [text] and/or an optional [key], adding it to a parent.
 * ```
 * ... {
 *  treeNode("text") // TreeNode component with text
 *  treeNode("text", key) // TreeNode with text and key
 * }
 * ```
 *
 * @param text The text of the `TreeNode`.
 * @param key The key of the `TreeNode`, if `null`, [text] is used.
 * @param block The initialization steps of the `TreeNode`.
 * @receiver The [TreeNode] which is the parent of `this TreeNode`.
 * @return The configured `TreeNode`.
 * @see TreeNode
 * @see tree
 * @see treeNode
 */
@WebforjDsl
fun @WebforjDsl TreeNode.treeNode(text: String, key: Any? = null, block: @WebforjDsl TreeNode.() -> Unit = {}): TreeNode {
  val node = key?.let { TreeNode(key, text) } ?: TreeNode(text)
  node.block()
  add(node)
  return node
}

@WebforjDsl
fun <T> @WebforjDsl TreeNode.iconSlot(block: @WebforjDsl HasComponents.() -> T) where T: Component, T: IconDefinition<*> {
  SingleSlotSetter(block).setSlot(this) {
    setIcon(it as IconDefinition<*>)
  }
}

@WebforjDsl
fun <T> @WebforjDsl TreeNode.selectedIconSlot(block: @WebforjDsl HasComponents.() -> T) where T: Component, T: IconDefinition<*> {
  SingleSlotSetter(block).setSlot(this) {
    setSelectedIcon(it as IconDefinition<*>)
  }
}

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

/**
 * Sets the icon to display for collapsed tree nodes.
 * ```
 * ... {
 *   tree {
 *     collapsedIconSlot {
 *       featherIcon(FeatherIcon.CHEVRON_RIGHT)
 *     }
 *   }
 * }
 * ```
 *
 * @param block The initialization steps for the icon.
 */
@WebforjDsl
fun <T> @WebforjDsl Tree.collapsedIconSlot(block: @WebforjDsl HasComponents.() -> T) where T: Component, T: IconDefinition<*> {
  SingleSlotSetter(block).setSlot(this) {
    setCollapsedIcon(it as IconDefinition<*>)
  }
}

/**
 * Sets the icon to display for expanded tree nodes.
 * ```
 * ... {
 *   tree {
 *     expandedIconSlot {
 *       featherIcon(FeatherIcon.CHEVRON_DOWN)
 *     }
 *   }
 * }
 * ```
 *
 * @param block The initialization steps for the icon.
 */
@WebforjDsl
fun <T> @WebforjDsl Tree.expandedIconSlot(block: @WebforjDsl HasComponents.() -> T) where T: Component, T: IconDefinition<*> {
  SingleSlotSetter(block).setSlot(this) {
    setExpandedIcon(it as IconDefinition<*>)
  }
}

/**
 * Sets the icon to display for leaf tree nodes.
 * ```
 * ... {
 *   tree {
 *     leafIconSlot {
 *       featherIcon(FeatherIcon.FILE)
 *     }
 *   }
 * }
 * ```
 *
 * @param block The initialization steps for the icon.
 */
@WebforjDsl
fun <T> @WebforjDsl Tree.leafIconSlot(block: @WebforjDsl HasComponents.() -> T) where T: Component, T: IconDefinition<*> {
  SingleSlotSetter(block).setSlot(this) {
    setLeafIcon(it as IconDefinition<*>)
  }
}

/**
 * Sets the icon to display for selected leaf tree nodes.
 * ```
 * ... {
 *   tree {
 *     leafSelectedIconSlot {
 *       featherIcon(FeatherIcon.FILE_PLUS)
 *     }
 *   }
 * }
 * ```
 *
 * @param block The initialization steps for the icon.
 */
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

/**
 * Sets the icon to display for this tree node.
 * ```
 * ... {
 *   tree {
 *     treeNode("Folder") {
 *       iconSlot {
 *         featherIcon(FeatherIcon.FOLDER)
 *       }
 *     }
 *   }
 * }
 * ```
 *
 * @param block The initialization steps for the icon.
 */
@WebforjDsl
fun <T> @WebforjDsl TreeNode.iconSlot(block: @WebforjDsl HasComponents.() -> T) where T: Component, T: IconDefinition<*> {
  SingleSlotSetter(block).setSlot(this) {
    setIcon(it as IconDefinition<*>)
  }
}

/**
 * Sets the icon to display for this tree node when selected.
 * ```
 * ... {
 *   tree {
 *     treeNode("Folder") {
 *       selectedIconSlot {
 *         featherIcon(FeatherIcon.FOLDER_PLUS)
 *       }
 *     }
 *   }
 * }
 * ```
 *
 * @param block The initialization steps for the icon.
 */
@WebforjDsl
fun <T> @WebforjDsl TreeNode.selectedIconSlot(block: @WebforjDsl HasComponents.() -> T) where T: Component, T: IconDefinition<*> {
  SingleSlotSetter(block).setSlot(this) {
    setSelectedIcon(it as IconDefinition<*>)
  }
}

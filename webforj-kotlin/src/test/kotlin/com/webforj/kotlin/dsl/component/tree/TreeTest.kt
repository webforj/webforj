package com.webforj.kotlin.dsl.component.tree

import com.basis.bbj.comm.al
import com.webforj.component.html.elements.Div
import com.webforj.component.tree.TreeNode
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.DisplayName
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertNotNull
import kotlin.test.assertTrue

class TreeTest {
  lateinit var root: Div

  @BeforeEach
  fun setup() {
    root = Div()
  }

  @Test
  @DisplayName("Create Tree with default values")
  fun shouldCreateDefaultTree() {
    val tree = root.tree()
    assertTrue { root.hasComponent(tree) }
    assertTrue { tree.children.isEmpty() }
  }

  @Test
  @DisplayName("Create Tree with child")
  fun shouldCreateTreeWithChild() {
    var node: TreeNode? = null
    val tree = root.tree {
      node = treeNode("child")
    }
    assertTrue { root.hasComponent(tree) }
    assertFalse { tree.children.isEmpty() }
    assertEquals("child", node?.text)
  }

  @Test
  @DisplayName("Create Tree with nested children")
  fun shouldCreateTreeWithNestedChildren() {
    var node: TreeNode? = null
    var grandchild: TreeNode? = null
    val tree = root.tree {
      node = treeNode("child") {
        grandchild = treeNode("grandchild")
      }
    }
    assertTrue { root.hasComponent(tree) }
    assertFalse { tree.children.isEmpty() }
    assertEquals("child", node?.text)
    assertEquals("grandchild", grandchild?.text)
  }

  @Test
  @DisplayName("Create TreeNode with key and text")
  fun shouldCreateTreeNodeWithKeyAndText() {
    var node: TreeNode? = null
    val tree = root.tree {
      node = treeNode("child", "key")
    }
    assertTrue { root.hasComponent(tree) }
    assertFalse { tree.children.isEmpty() }
    assertEquals("child", node?.text)
    assertEquals("key", node?.key)
  }

  @Test
  @DisplayName("Create nested TreeNode with key and text")
  fun shouldCreateNestedTreeNodeWithKeyAndText() {
    var node: TreeNode? = null
    val tree = root.tree {
      treeNode("child") {
        node = treeNode("grandchild", "key")
      }
    }
    assertTrue { root.hasComponent(tree) }
    assertFalse { tree.children.isEmpty() }
    assertEquals("grandchild", node?.text)
    assertEquals("key", node?.key)
  }

  @Test
  @DisplayName("Create Tree example")
  fun shouldCreateTreeExample() {
    val tree = root.tree {
      treeNode("Projects") {
        treeNode("Alpha") {
          treeNode("Planning")
          treeNode("Execution")
          treeNode("Review")
        }
        treeNode("Beta") {
          treeNode("Design")
          treeNode("Development")
          treeNode("Testing")
        }
      }
      treeNode("Departments") {
        treeNode("Engineering") {
          treeNode("Software")
          treeNode("Hardware")
        }
        treeNode("Marketing")
        treeNode("Human Resources")
      }
      expand("Departments")
    }
    val projectsNode = tree.children[0]
    assertNotNull(projectsNode)
    assertEquals("Projects", projectsNode.text)
    assertFalse { tree.isExpanded(projectsNode) }
    val alphaNode = projectsNode.children[0]
    assertNotNull(alphaNode)
    assertEquals("Alpha", alphaNode.key)
    assertFalse { tree.isExpanded(alphaNode) }
    val planningNode = alphaNode.children[0]
    assertNotNull(planningNode)
    assertEquals("Planning", planningNode.key)
    assertFalse { tree.isExpanded(planningNode) }
    val executionNode = alphaNode.children[1]
    assertNotNull(executionNode)
    assertEquals("Execution", executionNode.key)
    assertFalse { tree.isExpanded(executionNode) }
    val reviewNode = alphaNode.children[2]
    assertNotNull(reviewNode)
    assertEquals("Review", reviewNode.key)
    assertFalse { tree.isExpanded(reviewNode) }
    val betaNode = projectsNode.children[1]
    assertNotNull(betaNode)
    assertEquals("Beta", betaNode.key)
    assertFalse { tree.isExpanded(betaNode) }
    val designNode = betaNode.children[0]
    assertNotNull(designNode)
    assertEquals("Design", designNode.key)
    assertFalse { tree.isExpanded(designNode) }
    val developmentNode = betaNode.children[1]
    assertNotNull(developmentNode)
    assertEquals("Development", developmentNode.key)
    assertFalse { tree.isExpanded(developmentNode) }
    val testingNode = betaNode.children[2]
    assertNotNull(testingNode)
    assertEquals("Testing", testingNode.key)
    assertFalse { tree.isExpanded(testingNode) }
    val departmentsNode = tree.children[1]
    assertNotNull(departmentsNode)
    assertEquals("Departments", departmentsNode.key)
    assertTrue { tree.isExpanded(departmentsNode) }
    val engineeringNode = departmentsNode.children[0]
    assertNotNull(engineeringNode)
    assertEquals("Engineering", engineeringNode.key)
    assertFalse { tree.isExpanded(engineeringNode) }
    engineeringNode.children.zip(listOf("Software", "Hardware")).forEach { (node, key) ->
      assertEquals(key, node.key)
    }
    val marketingNode = departmentsNode.children[1]
    assertNotNull(engineeringNode)
    assertEquals("Marketing", marketingNode.key)
    val resourcesNode = departmentsNode.children[2]
    assertNotNull(resourcesNode)
    assertEquals("Human Resources", resourcesNode.key)
  }

}
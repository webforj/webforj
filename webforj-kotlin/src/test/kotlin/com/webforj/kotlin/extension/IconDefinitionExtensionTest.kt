package com.webforj.kotlin.extension

import com.webforj.component.html.elements.Div
import com.webforj.component.icons.DwcIcon
import com.webforj.component.icons.FeatherIcon
import com.webforj.component.icons.TablerIcon
import com.webforj.kotlin.dsl.build
import com.webforj.kotlin.dsl.component.infiniitescroll.infiniteScroll
import com.webforj.kotlin.dsl.component.refresher.refresher
import com.webforj.kotlin.dsl.component.tree.tree
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class IconDefinitionExtensionTest {
  lateinit var root: Div

  @BeforeEach
  fun setup() {
    root = Div()
  }

  @Test
  fun shouldCreateInfiniteScrollWithIcon() {
    root.build {
      val scroll = infiniteScroll {
        icon = FeatherIcon.FEATHER.create().toQualifiedName()
      }
      assertEquals(FeatherIcon.FEATHER.create().toQualifiedName(), scroll.icon)
    }
  }

  @Test
  fun shouldCreateRefresherWithArrowIcon() {
    root.build {
      val refresher = refresher {
        arrowIcon = DwcIcon.ARROW_DOWN.create().toQualifiedName()
      }
      assertEquals(DwcIcon.ARROW_DOWN.create().toQualifiedName(), refresher.arrowIcon)
    }
  }

  @Test
  fun shouldCreateTreeWithLeafIcon() {
    root.build {
      val tree = tree {
        leafIcon = TablerIcon.create("leaf").toQualifiedName()
      }
      assertEquals(TablerIcon.create("leaf").toQualifiedName(), tree.leafIcon)
    }
  }
}

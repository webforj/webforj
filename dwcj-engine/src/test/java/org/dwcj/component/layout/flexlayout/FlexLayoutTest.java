package org.dwcj.component.layout.flexlayout;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.dwcj.component.html.elements.Div;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

class FlexLayoutTest {

  private FlexLayout flexLayout;

  @BeforeEach
  void setUp() {
    flexLayout = new FlexLayout();
  }

  @Nested
  class ContainerApi {
    @Test
    void shouldUseInlineFlex() {
      flexLayout.setInline(true);
      assertTrue(flexLayout.isInline());
      assertEquals("inline-flex", flexLayout.getStyle("display"));

      flexLayout.setInline(false);
      assertFalse(flexLayout.isInline());
      assertEquals("flex", flexLayout.getStyle("display"));
    }

    @ParameterizedTest
    @EnumSource(FlexDirection.class)
    void shouldSetDirection(FlexDirection direction) {
      flexLayout.setDirection(direction);
      assertEquals(direction, flexLayout.getDirection());
      assertEquals(direction.getValue(), flexLayout.getStyle(FlexProperties.PROP_DIRECTION));
    }

    @ParameterizedTest
    @EnumSource(FlexWrap.class)
    void shouldSetWrap(FlexWrap wrap) {
      flexLayout.setWrap(wrap);
      assertEquals(wrap, flexLayout.getWrap());
      assertEquals(wrap.getValue(), flexLayout.getStyle(FlexProperties.PROP_WRAP));
    }

    @ParameterizedTest
    @EnumSource(FlexFlow.class)
    void shouldSetFlow(FlexFlow flow) {
      flexLayout.setFlow(flow);
      assertEquals(flow, flexLayout.getFlow());
      assertEquals(flow.getValue(), flexLayout.getStyle(FlexProperties.PROP_FLOW));
    }

    @ParameterizedTest
    @EnumSource(FlexJustifyContent.class)
    void shouldSetJustifyContent(FlexJustifyContent justifyContent) {
      flexLayout.setJustifyContent(justifyContent);
      assertEquals(justifyContent, flexLayout.getJustifyContent());
      assertEquals(justifyContent.getValue(),
          flexLayout.getStyle(FlexProperties.PROP_JUSTIFY_CONTENT));
    }

    @ParameterizedTest
    @EnumSource(FlexAlignment.class)
    void shouldSetAlignment(FlexAlignment alignment) {
      flexLayout.setAlignment(alignment);
      assertEquals(alignment, flexLayout.getAlignment());
      assertEquals(alignment.getValue(), flexLayout.getStyle(FlexProperties.PROP_ALIGN_ITEMS));
    }

    @ParameterizedTest
    @EnumSource(FlexContentAlignment.class)
    void shouldSetAlignContent(FlexContentAlignment alignContent) {
      flexLayout.setAlignContent(alignContent);
      assertEquals(alignContent, flexLayout.getAlignContent());
      assertEquals(alignContent.getValue(), flexLayout.getStyle(FlexProperties.PROP_ALIGN_CONTENT));
    }

    @Test
    void shouldSetSpacing() {
      flexLayout.setSpacing("2em");
      assertEquals("2em", flexLayout.getSpacing());
      assertEquals("2em", flexLayout.getStyle(FlexProperties.PROP_GAP));

      flexLayout.setSpacing(null);
      assertEquals("", flexLayout.getSpacing());
      assertNull(flexLayout.getStyle(FlexProperties.PROP_GAP));
    }

    @Test
    void shouldSetMargin() {
      flexLayout.setMargin("10px");
      assertEquals("10px", flexLayout.getMargin());
      assertEquals("10px", flexLayout.getStyle("margin"));
    }

    @Test
    void shouldSetPadding() {
      flexLayout.setPadding("5px");
      assertEquals("5px", flexLayout.getPadding());
      assertEquals("5px", flexLayout.getStyle("padding"));
    }
  }

  @Nested
  class ItemApi {
    @Test
    void shouldSetItemOrder() {
      Div item = new Div();
      flexLayout.add(item);

      flexLayout.setItemOrder(2, item);
      assertEquals(2, flexLayout.getItemOrder(item));
      assertEquals("2", item.getStyle(FlexProperties.PROP_ORDER));

      flexLayout.setItemOrder(0, item);
      assertEquals(0, flexLayout.getItemOrder(item));
      assertEquals("", item.getStyle(FlexProperties.PROP_ORDER));
    }

    @Test
    void shouldSetItemGrow() {
      Div item = new Div();
      flexLayout.add(item);

      flexLayout.setItemGrow(2.5, item);
      assertEquals(2.5, flexLayout.getItemGrow(item));
      assertEquals("2.5", item.getStyle(FlexProperties.PROP_GROW));

      flexLayout.setItemGrow(0, item);
      assertEquals(0, flexLayout.getItemGrow(item));
      assertEquals("", item.getStyle(FlexProperties.PROP_GROW));
    }

    @Test
    void shouldThrowExceptionWhenGrowIsNegative() {
      Div item = new Div();
      flexLayout.add(item);

      assertThrows(IllegalArgumentException.class, () -> flexLayout.setItemGrow(-1, item));
    }

    @Test
    void shouldSetItemShrink() {
      Div item = new Div();
      flexLayout.add(item);

      flexLayout.setItemShrink(1.5, item);
      assertEquals(1.5, flexLayout.getItemShrink(item));
      assertEquals("1.5", item.getStyle(FlexProperties.PROP_SHRINK));
    }

    @Test
    void shouldThrowExceptionWhenShrinkIsNegative() {
      Div item = new Div();
      flexLayout.add(item);

      assertThrows(IllegalArgumentException.class, () -> flexLayout.setItemShrink(-1, item));
    }

    @Test
    void shouldSetItemBasis() {
      Div item = new Div();
      flexLayout.add(item);

      flexLayout.setItemBasis("30%", item);
      assertEquals("30%", flexLayout.getItemBasis(item));
      assertEquals("30%", item.getStyle(FlexProperties.PROP_BASIS));

      flexLayout.setItemBasis(null, item);
      assertEquals("", flexLayout.getItemBasis(item));
      assertEquals("", item.getStyle(FlexProperties.PROP_BASIS));
    }

    @ParameterizedTest
    @EnumSource(FlexAlignment.class)
    void shouldSetItemAlignment(FlexAlignment alignment) {
      Div item = new Div();
      flexLayout.add(item);

      flexLayout.setItemAlignment(alignment, item);
      assertEquals(alignment, flexLayout.getItemAlignment(item));
      assertEquals(alignment.getValue(), item.getStyle(FlexProperties.PROP_ALIGN_SELF));
    }

    @Test
    void shouldRemoveItemAlignmentWhenNull() {
      Div item = new Div();
      flexLayout.add(item);

      flexLayout.setItemAlignment(null, item);
      assertEquals(FlexAlignment.getDefault(), flexLayout.getItemAlignment(item));
      assertEquals(FlexAlignment.getDefault().getValue(),
          item.getStyle(FlexProperties.PROP_ALIGN_SELF));
    }

    @Test
    void itemApiExceptionWhenNotChild() {
      Div item = new Div();

      assertThrows(IllegalArgumentException.class, () -> flexLayout.setItemOrder(1, item));
      assertThrows(IllegalArgumentException.class, () -> flexLayout.setItemGrow(1, item));
      assertThrows(IllegalArgumentException.class, () -> flexLayout.setItemShrink(1, item));
      assertThrows(IllegalArgumentException.class, () -> flexLayout.setItemBasis("1", item));
      assertThrows(IllegalArgumentException.class,
          () -> flexLayout.setItemAlignment(FlexAlignment.CENTER, item));
    }

    @Test
    void itemApiExceptionWhenNull() {
      assertThrows(NullPointerException.class, () -> flexLayout.setItemOrder(1, null));
      assertThrows(NullPointerException.class, () -> flexLayout.setItemGrow(1, null));
      assertThrows(NullPointerException.class, () -> flexLayout.setItemShrink(1, null));
      assertThrows(NullPointerException.class, () -> flexLayout.setItemBasis("1", null));
      assertThrows(NullPointerException.class,
          () -> flexLayout.setItemAlignment(FlexAlignment.CENTER, null));
    }
  }
}

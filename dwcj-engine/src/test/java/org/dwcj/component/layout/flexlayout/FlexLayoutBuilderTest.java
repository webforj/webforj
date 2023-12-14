package org.dwcj.component.layout.flexlayout;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.dwcj.component.html.elements.Div;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

class FlexLayoutBuilderTest {

  private FlexLayoutBuilder builder;

  @BeforeEach
  void setUp() {
    builder = new FlexLayoutBuilder();
  }

  @Nested
  class HorizontalLayout {
    @Test
    void shouldCreateHorizontalLayout() {
      FlexLayoutBuilder result = builder.horizontal();
      assertFalse(result.isInline());
      assertEquals(FlexDirection.ROW, result.getDirection());
    }

    @Test
    void shouldCreateInlineHorizontalLayout() {
      FlexLayoutBuilder result = builder.horizontal(true);
      assertTrue(result.isInline());
      assertEquals(FlexDirection.ROW, result.getDirection());
    }

    @Test
    void shouldCreateHorizontalReverseLayout() {
      FlexLayoutBuilder result = builder.horizontalReverse();
      assertFalse(result.isInline());
      assertEquals(FlexDirection.ROW_REVERSE, result.getDirection());
    }

    @Test
    void shouldCreateInlineHorizontalReverseLayout() {
      FlexLayoutBuilder result = builder.horizontalReverse(true);
      assertTrue(result.isInline());
      assertEquals(FlexDirection.ROW_REVERSE, result.getDirection());
    }
  }

  @Nested
  class VerticalLayout {
    @Test
    void shouldCreateVerticalLayout() {
      FlexLayoutBuilder result = builder.vertical();
      assertFalse(result.isInline());
      assertEquals(FlexDirection.COLUMN, result.getDirection());
    }

    @Test
    void shouldCreateInlineVerticalLayout() {
      FlexLayoutBuilder result = builder.vertical(true);
      assertTrue(result.isInline());
      assertEquals(FlexDirection.COLUMN, result.getDirection());
    }

    @Test
    void shouldCreateVerticalReverseLayout() {
      FlexLayoutBuilder result = builder.verticalReverse();
      assertFalse(result.isInline());
      assertEquals(FlexDirection.COLUMN_REVERSE, result.getDirection());
    }

    @Test
    void shouldCreateInlineVerticalReverseLayout() {
      FlexLayoutBuilder result = builder.verticalReverse(true);
      assertTrue(result.isInline());
      assertEquals(FlexDirection.COLUMN_REVERSE, result.getDirection());
    }
  }

  @Nested
  class Alignment {
    private FlexLayoutBuilder setAlignment(FlexLayoutBuilder builder, FlexAlignment alignment) {
      switch (alignment) {
        case START:
          return builder.align().start();
        case CENTER:
          return builder.align().center();
        case END:
          return builder.align().end();
        case STRETCH:
          return builder.align().stretch();
        case BASELINE:
          return builder.align().baseline();
        case AUTO:
          return builder.align().auto();
        default:
          throw new IllegalArgumentException("Invalid alignment: " + alignment);
      }
    }

    @ParameterizedTest
    @EnumSource(FlexAlignment.class)
    void shouldConfigureAlignment(FlexAlignment alignment) {
      FlexLayoutBuilder result = setAlignment(builder, alignment);
      assertEquals(alignment, result.getAlignment());
    }
  }

  @Nested
  class ContentAlignment {
    private FlexLayoutBuilder setContentAlignment(FlexLayoutBuilder builder,
        FlexContentAlignment contentAlignment) {
      switch (contentAlignment) {
        case NORMAL:
          return builder.contentAlign().normal();
        case START:
          return builder.contentAlign().start();
        case END:
          return builder.contentAlign().end();
        case CENTER:
          return builder.contentAlign().center();
        case BETWEEN:
          return builder.contentAlign().between();
        case AROUND:
          return builder.contentAlign().around();
        case EVENLY:
          return builder.contentAlign().evenly();
        case STRETCH:
          return builder.contentAlign().stretch();
        default:
          throw new IllegalArgumentException("Invalid content alignment: " + contentAlignment);
      }
    }

    @ParameterizedTest
    @EnumSource(FlexContentAlignment.class)
    void shouldConfigureContentAlignment(FlexContentAlignment contentAlignment) {
      FlexLayoutBuilder result = setContentAlignment(builder, contentAlignment);
      assertEquals(contentAlignment, result.getContentAlignment());
    }
  }

  @Nested
  class JustifyContent {
    private FlexLayoutBuilder setJustifyContent(FlexLayoutBuilder builder,
        FlexJustifyContent justifyContent) {
      switch (justifyContent) {
        case START:
          return builder.justify().start();
        case CENTER:
          return builder.justify().center();
        case END:
          return builder.justify().end();
        case BETWEEN:
          return builder.justify().between();
        case AROUND:
          return builder.justify().around();
        case EVENLY:
          return builder.justify().evenly();
        default:
          throw new IllegalArgumentException("Invalid justify content: " + justifyContent);
      }
    }

    @ParameterizedTest
    @EnumSource(FlexJustifyContent.class)
    void shouldConfigureJustifyContent(FlexJustifyContent justifyContent) {
      FlexLayoutBuilder result = setJustifyContent(builder, justifyContent);
      assertEquals(justifyContent, result.getJustifyContent());
    }
  }

  @Nested
  class Wrap {
    private FlexLayoutBuilder setWrap(FlexLayoutBuilder builder, FlexWrap wrap) {
      switch (wrap) {
        case WRAP:
          return builder.wrap();
        case NOWRAP:
          return builder.nowrap();
        case WRAP_REVERSE:
          return builder.wrapReverse();
        default:
          throw new IllegalArgumentException("Invalid wrap: " + wrap);
      }
    }

    @ParameterizedTest
    @EnumSource(FlexWrap.class)
    void shouldConfigureWrap(FlexWrap wrap) {
      FlexLayoutBuilder result = setWrap(builder, wrap);
      assertEquals(wrap, result.getWrap());
    }
  }

  @Test
  void shouldBuildLayout() {
    Div item = new Div();
    FlexLayout flexLayout = FlexLayout.create(item).horizontal() // direction
        .align().center() // alignment
        .contentAlign().center() // content alignment
        .justify().center() // justify content
        .wrap() // wrap content
        .build();

    assertNotNull(flexLayout);
    assertTrue(flexLayout.hasComponent(item));
    assertEquals(FlexDirection.ROW, flexLayout.getDirection());
    assertEquals(FlexAlignment.CENTER, flexLayout.getAlignment());
    assertEquals(FlexContentAlignment.CENTER, flexLayout.getAlignContent());
    assertEquals(FlexJustifyContent.CENTER, flexLayout.getJustifyContent());
    assertEquals(FlexWrap.WRAP, flexLayout.getWrap());
  }
}

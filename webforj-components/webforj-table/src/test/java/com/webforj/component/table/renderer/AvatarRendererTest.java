package com.webforj.component.table.renderer;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.component.avatar.AvatarShape;
import com.webforj.component.avatar.AvatarTheme;
import org.junit.jupiter.api.Test;

class AvatarRendererTest {

  @Test
  void shouldBuildDefault() {
    AvatarRenderer<String> r = new AvatarRenderer<>();
    String html = r.build();
    assertTrue(html.contains("<dwc-avatar"));
    assertTrue(html.contains("label='<%= cell.value %>'"));
    assertTrue(html.contains("expanse=''"));
  }

  @Test
  void shouldBuildWithLabel() {
    AvatarRenderer<String> r = new AvatarRenderer<>("John Doe");
    String html = r.build();
    assertTrue(html.contains("label='John Doe'"));
    assertTrue(html.contains("initials='JD'"));
  }

  @Test
  void shouldBuildWithExplicitInitials() {
    AvatarRenderer<String> r = new AvatarRenderer<>("John Doe", "X");
    String html = r.build();
    assertTrue(html.contains("initials='X'"));
    assertFalse(html.contains("initials='JD'"));
  }

  @Test
  void shouldBuildWithClientSideInitials() {
    AvatarRenderer<String> r = new AvatarRenderer<>();
    String html = r.build();
    assertTrue(html.contains("label='<%= cell.value %>'"));
    assertTrue(html.contains(
        "initials='<%= cell.value.trim().split(' ').filter(s => s).map(s => s[0]).join('') %>'"));
  }

  @Test
  void shouldBuildWithSrc() {
    AvatarRenderer<String> r = new AvatarRenderer<>("John Doe");
    r.setSrc("https://example.com/avatar.png");
    assertTrue(r.build().contains("src='https://example.com/avatar.png'"));
  }

  @Test
  void shouldBuildWithTheme() {
    AvatarRenderer<String> r = new AvatarRenderer<>("John Doe");
    r.setTheme(AvatarTheme.PRIMARY);
    assertTrue(r.build().contains("theme='primary'"));
  }

  @Test
  void shouldBuildWithShape() {
    AvatarRenderer<String> r = new AvatarRenderer<>("John Doe");
    r.setShape(AvatarShape.SQUARE);
    assertTrue(r.build().contains("shape='square'"));
  }

  @Test
  void shouldBuildWithIcon() {
    AvatarRenderer<String> r = new AvatarRenderer<>("John Doe");
    r.setIcon(new TestIconDefinition("user", "tabler"));
    assertTrue(r.build().contains("<dwc-icon name='user' pool='tabler'></dwc-icon>"));
  }

  @Test
  void shouldNotBuildIconWhenNull() {
    AvatarRenderer<String> r = new AvatarRenderer<>("John Doe");
    r.setIcon(new TestIconDefinition("user", "tabler"));
    r.setIcon(null);
    assertNull(r.getIcon());
    assertFalse(r.build().contains("dwc-icon"));
  }

  @Test
  void shouldBuildWithTemplateExpressionInSrc() {
    AvatarRenderer<String> r = new AvatarRenderer<>("John Doe");
    r.setSrc("https://example.com/avatars/<%= cell.value %>.png");
    assertTrue(r.build().contains("src='https://example.com/avatars/<%= cell.value %>.png'"));
  }
}

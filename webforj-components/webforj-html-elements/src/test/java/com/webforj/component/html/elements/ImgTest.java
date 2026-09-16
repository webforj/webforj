package com.webforj.component.html.elements;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.element.PropertyDescriptorTester;
import org.junit.jupiter.api.Test;

class ImgTest {

  @Test
  void shouldConstructWithSrcAndAlt() {
    String src = "https://example.com";
    String alt = "Example";

    Img img = new Img(src, alt);
    assertEquals(src, img.getSrc());
    assertEquals(alt, img.getAlt());
  }

  @Test
  void shouldConstructWithSrc() {
    String src = "https://example.com";

    Img img = new Img(src);
    assertEquals(src, img.getSrc());
  }

  @Test
  void shouldSetGetProperties() {
    Img component = new Img();

    assertDoesNotThrow(() -> {
      PropertyDescriptorTester.run(Img.class, component);
    });
  }
}

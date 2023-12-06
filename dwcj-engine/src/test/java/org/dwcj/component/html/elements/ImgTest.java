package org.dwcj.component.html.elements;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import org.dwcj.component.element.PropertyDescriptorTester;
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

    try {
      PropertyDescriptorTester.run(Img.class, component);
    } catch (Exception e) {
      fail("PropertyDescriptor test failed: " + e.getMessage());
    }
  }
}

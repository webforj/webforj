package com.webforj;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.net.MalformedURLException;
import java.net.URL;
import org.junit.jupiter.api.Test;

class RedirectActionTest {

  @Test
  void testConstructorWithValidUrlString() {
    String validUrl = "https://example.com";
    RedirectAction action = new RedirectAction(validUrl);
    URL url = action.getUrl();

    assertNotNull(url);
    assertEquals(validUrl, url.toString());
  }

  @Test
  void testConstructorWithValidUrl() throws MalformedURLException {
    String validUrl = "https://example.com";
    RedirectAction action = new RedirectAction(new URL(validUrl));
    URL url = action.getUrl();

    assertNotNull(url);
    assertEquals(validUrl, url.toString());
  }

  @Test
  void testConstructorWithInvalidUrl() {
    String invalidUrl = "not_a_valid_url";
    assertThrows(IllegalStateException.class, () -> new RedirectAction(invalidUrl));
  }
}

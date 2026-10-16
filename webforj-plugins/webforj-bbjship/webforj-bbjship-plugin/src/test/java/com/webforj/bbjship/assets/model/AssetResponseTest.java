package com.webforj.bbjship.assets.model;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;

final class AssetResponseTest {

  @Test
  void shouldStartWithTheStatusAndNoContent() {
    AssetResponse response = AssetResponse.create(404).build();

    assertEquals(404, response.getStatus());
    assertNull(response.getContentType());
    assertNull(response.getEntityTag());
    assertNull(response.getCacheControl());
    assertArrayEquals(new byte[0], response.getBody());
  }

  @Test
  void shouldCarryEveryValue() {
    AssetResponse response = AssetResponse.create(200).setContentType("text/css")
        .setEntityTag("\"tag\"").setCacheControl("max-age=60").setBody(new byte[] {1, 2}).build();

    assertEquals(200, response.getStatus());
    assertEquals("text/css", response.getContentType());
    assertEquals("\"tag\"", response.getEntityTag());
    assertEquals("max-age=60", response.getCacheControl());
    assertArrayEquals(new byte[] {1, 2}, response.getBody());
  }

  @Test
  void shouldCopyTheBodyOnTheWayInAndOut() {
    byte[] body = {1, 2};
    AssetResponse response = AssetResponse.create(200).setBody(body).build();

    body[0] = 9;
    response.getBody()[1] = 9;

    assertArrayEquals(new byte[] {1, 2}, response.getBody());
  }

  @Test
  void shouldRejectNullBody() {
    AssetResponse.Builder builder = AssetResponse.create(200);
    byte[] body = null;

    assertThrows(NullPointerException.class, () -> builder.setBody(body));
  }
}

package com.webforj.bbjship.assets.model;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

final class AssetRequestTest {

  private static final String APP_NAME = "sample";
  private static final String ICON = "icons/icon.png";

  @Test
  void shouldStartWithTheApplicationAndPathAndNoParameters() {
    AssetRequest request = AssetRequest.create(APP_NAME, ICON).build();

    assertEquals(APP_NAME, request.getApplicationName());
    assertEquals(ICON, request.getPath());
    assertNull(request.getPadding());
    assertNull(request.getBackground());
    assertNull(request.getIfNoneMatch());
  }

  @Test
  void shouldCarryEveryParameter() {
    AssetRequest request = AssetRequest.create(APP_NAME, "icons/icon-16x16.png").setPadding(".5")
        .setBackground("00ff00").setIfNoneMatch("\"tag\"").build();

    assertEquals(".5", request.getPadding());
    assertEquals("00ff00", request.getBackground());
    assertEquals("\"tag\"", request.getIfNoneMatch());
  }

  @Test
  void shouldAcceptNullParameters() {
    AssetRequest request = AssetRequest.create(APP_NAME, ICON).setPadding(".5").setPadding(null)
        .setBackground(null).setIfNoneMatch(null).build();

    assertNull(request.getPadding());
    assertNull(request.getBackground());
    assertNull(request.getIfNoneMatch());
  }

  @ParameterizedTest
  @MethodSource("getMissingValues")
  void shouldRejectMissingApplicationNameOrPath(String applicationName, String path) {
    assertThrows(NullPointerException.class, () -> AssetRequest.create(applicationName, path));
  }

  private static Stream<Arguments> getMissingValues() {
    return Stream.of(Arguments.of(null, ICON), Arguments.of(APP_NAME, null));
  }
}

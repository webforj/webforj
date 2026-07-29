package com.webforj.devtools.craftforj.security;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class ChannelCredentialsTest {

  @Test
  @DisplayName("Should draw a distinct nonce and sink id every time")
  void shouldDrawDistinctTokens() {
    ChannelCredentials first = ChannelCredentials.create();
    ChannelCredentials second = ChannelCredentials.create();

    assertNotEquals(first.getNonce(), first.getSinkId());
    assertNotEquals(first.getNonce(), second.getNonce());
    assertNotEquals(first.getSinkId(), second.getSinkId());
  }

  @Test
  @DisplayName("Should name a sink id that is safe to use as a property name")
  void shouldDrawIdentifierSafeSinkId() {
    for (int i = 0; i < 50; i++) {
      assertTrue(ChannelCredentials.create().getSinkId().matches("[A-Za-z0-9]{32}"));
    }
  }

  @Test
  @DisplayName("Should match only the nonce it was created with")
  void shouldMatchOnlyItsOwnNonce() {
    ChannelCredentials credentials = ChannelCredentials.of("secret", "sink");

    assertTrue(credentials.matches("secret"));
    assertFalse(credentials.matches("secre"));
    assertFalse(credentials.matches("secret "));
    assertFalse(credentials.matches(""));
    assertFalse(credentials.matches(null));
  }
}

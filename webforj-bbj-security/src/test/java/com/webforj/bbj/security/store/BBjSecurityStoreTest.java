package com.webforj.bbj.security.store;

import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.mockito.Mockito.mock;

import com.webforj.Environment;
import org.junit.jupiter.api.Test;

class BBjSecurityStoreTest {

  @Test
  void shouldCreateTheStoreOfTheBrowser() {
    BBjSecurityStore store = BBjSecurityStore.create(mock(Environment.class));

    assertInstanceOf(BBjBrowserSecurityStore.class, store);
  }
}

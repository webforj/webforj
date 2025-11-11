package com.webforj.webstorage;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.BBjThinClient;
import com.basis.startup.type.BBjException;
import com.webforj.exceptions.WebforjRuntimeException;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class CookieStorageTest {

  BBjThinClient thinClient;
  WebStorage webStorage;

  @BeforeEach
  void setUp() {
    thinClient = mock(BBjThinClient.class);
    webStorage = new CookieStorage(thinClient);
  }

  @Nested
  class AddApi {
    @Test
    void shouldAddItem() throws BBjException {
      webStorage.setItem("key1", "value1");

      verify(thinClient).setUserProperty(BBjThinClient.USER_PROPERTIES_COOKIES, "", "key1",
          "value1");
    }

    @Test
    void addItemShouldThrowException() throws BBjException {
      doThrow(BBjException.class).when(thinClient).setUserProperty(anyLong(), anyString(),
          anyString(), anyString());

      assertThrows(WebforjRuntimeException.class, () -> webStorage.setItem("key1", "value1"));
    }

    @Test
    void shouldAddMapOfItems() throws BBjException {
      webStorage.add(Map.of("key1", "value1", "key2", "value2"));
      verify(thinClient).setUserProperties(BBjThinClient.USER_PROPERTIES_COOKIES, "",
          Map.of("key1", "value1", "key2", "value2"));
    }

    @Test
    @SuppressWarnings("squid:S5778")
    void addMapOfItemsShouldThrowException() throws BBjException {
      doThrow(BBjException.class).when(thinClient).setUserProperties(anyLong(), anyString(),
          any(Map.class));

      assertThrows(WebforjRuntimeException.class,
          () -> webStorage.add(Map.of("key1", "value1", "key2", "value2")));
    }
  }

  @Nested
  class GetApi {
    @Test
    void shouldGetItem() throws BBjException {
      when(thinClient.getUserProperty(anyLong(), eq("key1"))).thenReturn("value1");

      String result = webStorage.getItem("key1");
      verify(thinClient).getUserProperty(BBjThinClient.USER_PROPERTIES_COOKIES, "key1");

      assertEquals("value1", result);
    }

    @Test
    void shouldGetMapOfItems() throws BBjException {
      List<String> keys = List.of("key1", "key2");
      when(thinClient.getUserProperties(anyLong(), eq(keys)))
          .thenReturn(Map.of("key1", "value1", "key2", "value2"));

      webStorage.get(keys);
      verify(thinClient).getUserProperties(BBjThinClient.USER_PROPERTIES_COOKIES, keys);
    }

    @Test
    void shouldReturnNullForNonExistentKey() throws BBjException {
      when(thinClient.getUserProperty(anyLong(), eq("nonexistent"))).thenThrow(BBjException.class);

      String result = webStorage.getItem("nonexistent");

      assertNull(result);
    }
  }

  @Test
  void shouldRemoveItem() throws BBjException {
    webStorage.removeItem("key1");
    verify(thinClient).setUserProperty(BBjThinClient.USER_PROPERTIES_COOKIES, "key1", null);
  }

  @Test
  void shouldClearStorage() throws BBjException {
    webStorage.clear();
    verify(thinClient).clearUserProperties(BBjThinClient.USER_PROPERTIES_COOKIES);
  }
}

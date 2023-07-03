package org.dwcj;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.basis.bbj.proxies.BBjThinClient;
import com.basis.startup.type.BBjException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import org.dwcj.AbstractWebStorage.PropertySamesite;
import org.dwcj.AbstractWebStorage.WebStorageType;
import org.dwcj.exceptions.DwcjRuntimeException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class AbstractWebStorageTest {
  BBjThinClient thinClient = mock(BBjThinClient.class);
  AbstractWebStorage webStorage = new AbstractWebStorageMock(thinClient);

  @Test
  void protectedAddWithSamesite() {
    webStorage.add(PropertySamesite.SAME_SITE_STRICT, "key", "value");

    try {
      verify(thinClient, times(1)).setUserProperty(WebStorageType.COOKIES.getValue(),
          PropertySamesite.SAME_SITE_STRICT.getValue(), "key", "value");
    } catch (BBjException e) {
      //
    }
  }

  @Test
  void protectedAddMultipleWithSamesite() {
    final HashMap<String, String> values = new HashMap<>();

    values.put("key1", "value1");
    values.put("key2", "value2");
    values.put("key3", "value3");
    values.put("key4", "value4");

    webStorage.add(PropertySamesite.SAME_SITE_STRICT, values);

    try {
      verify(thinClient, times(1)).setUserProperties(WebStorageType.COOKIES.getValue(),
          PropertySamesite.SAME_SITE_STRICT.getValue(), values);
    } catch (Exception e) {
      //
    }
  }

  @Test
  void addWithKeyAndValue() {
    webStorage.add("key", "value");

    try {
      verify(thinClient, times(1)).setUserProperty(WebStorageType.COOKIES.getValue(),
          PropertySamesite.SAME_SITE_DEFAULT.getValue(), "key", "value");
    } catch (BBjException e) {
      //
    }
  }

  @Test
  void addWithMultipleValues() {
    final HashMap<String, String> values = new HashMap<>();

    values.put("key1", "value1");
    values.put("key2", "value2");
    values.put("key3", "value3");
    values.put("key4", "value4");

    webStorage.add(values);

    try {
      verify(thinClient, times(1)).setUserProperties(WebStorageType.COOKIES.getValue(),
          PropertySamesite.SAME_SITE_DEFAULT.getValue(), values);
    } catch (Exception e) {
      //
    }
  }

  @Test
  void getStorageValue() {
    webStorage.get("key");

    try {
      verify(thinClient, times(1)).getUserProperty(WebStorageType.COOKIES.getValue(), "key");
    } catch (Exception e) {
      //
    }

  }

  @Test
  void getWithMultipleKeys() {
    final List<String> values = new ArrayList<>();

    values.add("key1");
    values.add("key2");
    values.add("key3");
    values.add("key4");

    webStorage.get(values);

    try {
      verify(thinClient, times(1)).getUserProperties(WebStorageType.COOKIES.getValue(), values);
    } catch (Exception e) {
      //
    }
  }


  @Test
  void removeFromStorage() {
    webStorage.remove("key");

    try {
      verify(thinClient, times(1)).setUserProperty(WebStorageType.COOKIES.getValue(), "key", null);
    } catch (Exception e) {
      //
    }
  }

  @Test
  void removeMultiple() {
    final List<String> values = new ArrayList<>();

    values.add("key1");
    values.add("key2");
    values.add("key3");
    values.add("key4");

    webStorage.remove(values);

    try {
      verify(thinClient, times(4)).setUserProperty(WebStorageType.COOKIES.getValue(), anyString(),
          null);
    } catch (Exception e) {
      //
    }
  }

  @Test
  void clear() {
    webStorage.clear();

    try {
      verify(thinClient, times(1)).clearUserProperties(WebStorageType.COOKIES.getValue());
    } catch (Exception e) {
      //
    }
  }

  @Test
  void addExceptionPath() throws BBjException {
    doThrow(BBjException.class).when(thinClient).setUserProperty(WebStorageType.COOKIES.getValue(),
        PropertySamesite.SAME_SITE_DEFAULT.getValue(), "key", "value");

    assertThrows(DwcjRuntimeException.class, () -> webStorage.add("key", "value"));
  }


  @Test
  void addMultipleExceptionPath() throws BBjException {
    final HashMap<String, String> values = new HashMap<>();

    values.put("key1", "value1");
    values.put("key2", "value2");
    values.put("key3", "value3");
    values.put("key4", "value4");

    doThrow(BBjException.class).when(thinClient).setUserProperties(
        WebStorageType.COOKIES.getValue(), PropertySamesite.SAME_SITE_DEFAULT.getValue(), values);

    assertThrows(DwcjRuntimeException.class, () -> webStorage.add(values));
  }

  @Test
  void getExceptionPath() throws BBjException {
    doThrow(BBjException.class).when(thinClient).getUserProperty(WebStorageType.COOKIES.getValue(),
        "key");

    assertNull(webStorage.get("key"));
  }

  @Test
  void getMultipleExceptionPath() throws BBjException {
    final List<String> values = new ArrayList<>();

    values.add("key1");
    values.add("key2");
    values.add("key3");
    values.add("key4");

    doThrow(BBjException.class).when(thinClient)
        .getUserProperties(WebStorageType.COOKIES.getValue(), values);

    assertTrue(webStorage.get(values).isEmpty());
  }

  @Test
  void removeExceptionPath() throws BBjException {
    doThrow(BBjException.class).when(thinClient).setUserProperty(WebStorageType.COOKIES.getValue(),
        "key", null);

    assertThrows(DwcjRuntimeException.class, () -> webStorage.remove("key"));
  }

  @Test
  void clearWithException() throws BBjException {
    doThrow(BBjException.class).when(thinClient)
        .clearUserProperties(WebStorageType.COOKIES.getValue());

    assertThrows(DwcjRuntimeException.class, () -> webStorage.clear());
  }



}

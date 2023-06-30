package org.dwcj;

import static org.mockito.ArgumentMatchers.anyCollection;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.basis.bbj.proxies.BBjThinClient;
import com.basis.startup.type.BBjException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import org.dwcj.Page.PropertyGroup;
import org.dwcj.Page.PropertySamesite;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
public class PageTest {

  BBjThinClient thinClient = mock(BBjThinClient.class);
  Page page = new Page(thinClient);

  @Nested
  @DisplayName("User properties Api")
  class UserPropertiesApi {

    @Test
    @DisplayName("set userproperty with 4 params")
    void setUserPropertyWith4Parameters() {

      page.setUserProperty(PropertyGroup.USER_PROPERTIES_SESSION, PropertySamesite.SAME_SITE_STRICT,
          "key", "value");

      try {
        verify(thinClient, times(1)).setUserProperty(BBjThinClient.USER_PROPERTIES_SESSION,
            BBjThinClient.SAME_SITE_STRICT, "key", "value");
      } catch (BBjException e) {
        //
      }
    }

    @Test
    @DisplayName("set userproperty with 3 params")
    void setUserPropertyWith3Parameters() {

      page.setUserProperty(PropertyGroup.USER_PROPERTIES_SESSION, "key", "value");

      try {
        verify(thinClient, times(1)).setUserProperty(BBjThinClient.USER_PROPERTIES_SESSION, "key",
            "value");
      } catch (BBjException e) {
        //
      }
    }

    @Test
    @DisplayName("set userproperty with 2 params")
    void setUserPropertyWith2Parameters() {

      page.setUserProperty("key", "value");

      try {
        verify(thinClient, times(1)).setUserProperty("key", "value");
      } catch (BBjException e) {
        //
      }
    }

    @Test
    @DisplayName("get user property with group")
    void getUserPropertyWithGroup() {

      page.getUserProperty(PropertyGroup.USER_PROPERTIES_SESSION, "key");

      try {
        verify(thinClient, times(1)).getUserProperty(BBjThinClient.USER_PROPERTIES_SESSION, "key");
      } catch (BBjException e) {
        //
      }
    }

    @Test
    @DisplayName("get user property")
    void getUserProperty() {

      page.getUserProperty("key");

      try {
        verify(thinClient, times(1)).getUserProperty("key");
      } catch (BBjException e) {
        //
      }
    }

    @Test
    @DisplayName("clearUserProperties")
    void clearUserProperties() {

      page.clearUserProperties();

      try {
        verify(thinClient, times(1)).clearUserProperties();
      } catch (BBjException e) {
        //
      }
    }

    @Test
    @DisplayName("clearUserProperties with group")
    void clearUserPropertiesWithGroup() {

      page.clearUserProperties(PropertyGroup.USER_PROPERTIES_STORAGE);

      try {
        verify(thinClient, times(1)).clearUserProperties(BBjThinClient.USER_PROPERTIES_STORAGE);
      } catch (BBjException e) {
        //
      }
    }

    @Test
    @DisplayName("getUserProperties")
    void getUserProperties() {

      page.getUserProperties();

      try {
        verify(thinClient, times(1)).getUserProperties();
      } catch (BBjException e) {
        //
      }
    }


    @Test
    @DisplayName("getUserProperties with keys")
    void getUserPropertiesWithKeys() {

      page.getUserProperties(anyCollection());

      try {
        verify(thinClient, times(1)).getUserProperties(anyCollection());
      } catch (BBjException e) {
        //
      }
    }


    @Test
    @DisplayName("getUserProperties with group")
    void getUserPropertiesWithGroup() {

      page.getUserProperties(PropertyGroup.USER_PROPERTIES_SESSION);

      try {
        verify(thinClient, times(1)).getUserProperties(BBjThinClient.USER_PROPERTIES_SESSION);
      } catch (BBjException e) {
        //
      }
    }

    @Test
    @DisplayName("getUserProperties with group and keys")
    void getUserPropertiesWithGroupAndKeys() {

      Collection<String> collection = new ArrayList<String>();

      collection.add("key1");
      collection.add("key2");
      collection.add("key3");
      collection.add("key4");

      page.getUserProperties(PropertyGroup.USER_PROPERTIES_SESSION, collection);

      try {
        verify(thinClient, times(1)).getUserProperties(BBjThinClient.USER_PROPERTIES_SESSION,
            collection);
      } catch (BBjException e) {
        //
      }
    }


    @Test
    @DisplayName("setUserProperties")
    void setUserProperties() {

      final HashMap<String, String> properties = new HashMap<>();
      properties.put("key", "value");
      properties.put("key1", "value1");
      properties.put("key2", "value2");

      page.setUserProperties(properties);

      try {
        verify(thinClient, times(1)).setUserProperties(properties);
      } catch (BBjException e) {
        //
      }
    }

    @Test
    @DisplayName("setUserProperties with group")
    void setUserPropertiesWithGroup() {

      final HashMap<String, String> properties = new HashMap<>();
      properties.put("key", "value");
      properties.put("key1", "value1");
      properties.put("key2", "value2");

      page.setUserProperties(PropertyGroup.USER_PROPERTIES_COOKIES, properties);

      try {
        verify(thinClient, times(1)).setUserProperties(BBjThinClient.USER_PROPERTIES_COOKIES,
            properties);
      } catch (BBjException e) {
        //
      }
    }

    @Test
    @DisplayName("setUserProperties with group and samesite")
    void setUserPropertiesWithGroupAndSamesite() {

      final HashMap<String, String> properties = new HashMap<>();
      properties.put("key", "value");
      properties.put("key1", "value1");
      properties.put("key2", "value2");

      page.setUserProperties(PropertyGroup.USER_PROPERTIES_COOKIES, PropertySamesite.SAME_SITE_LAX,
          properties);

      try {
        verify(thinClient, times(1)).setUserProperties(BBjThinClient.USER_PROPERTIES_COOKIES,
            BBjThinClient.SAME_SITE_LAX, properties);
      } catch (BBjException e) {
        //
      }
    }

  }

}

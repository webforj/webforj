package com.webforj.devtools.craftforj.appinfo.action;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.appinfo.AppInfoCollector;
import com.webforj.devtools.craftforj.appinfo.model.AppInfo;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class GetAppInfoActionTest {

  @Test
  @DisplayName("Should return correct action name")
  void shouldReturnCorrectActionName() {
    GetAppInfoAction action = new GetAppInfoAction(mock(AppInfoCollector.class));
    assertEquals("appinfo.getAppInfo", action.getAction());
  }

  @Test
  @DisplayName("Should return collected info")
  void shouldReturnCollectedInfo() {
    AppInfoCollector collector = mock(AppInfoCollector.class);
    AppInfo info = new AppInfo();
    when(collector.collect()).thenReturn(info);

    GetAppInfoAction action = new GetAppInfoAction(collector);

    assertSame(info, action.handle(new JsonObject()));
    assertSame(info, action.handle(null));
  }
}

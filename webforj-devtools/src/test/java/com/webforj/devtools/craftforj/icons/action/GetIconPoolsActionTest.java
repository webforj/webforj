package com.webforj.devtools.craftforj.icons.action;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.component.icons.DwcIcon;
import com.webforj.component.icons.FeatherIcon;
import com.webforj.devtools.craftforj.icons.action.GetIconPoolsAction.PoolInfo;
import java.util.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("GetIconPoolsAction")
class GetIconPoolsActionTest {

  private final GetIconPoolsAction action = new GetIconPoolsAction();

  @Test
  @DisplayName("uses the icons.getPools action name")
  void shouldUseActionName() {
    assertEquals("icons.getPools", action.getAction());
  }

  @Test
  @DisplayName("lists the supported pools in order")
  void shouldListSupportedPools() {
    List<PoolInfo> pools = action.handle(null).getPools();

    assertEquals(List.of("tabler", "feather", "dwc"),
        pools.stream().map(PoolInfo::getName).toList());
  }

  @Test
  @DisplayName("leaves tabler without a name list")
  void shouldLeaveTablerWithoutNames() {
    List<PoolInfo> pools = action.handle(null).getPools();

    assertNull(pools.get(0).getNames());
  }

  @Test
  @DisplayName("enumerates feather and dwc names from the icon enums")
  void shouldEnumerateEnumBackedPools() {
    List<PoolInfo> pools = action.handle(null).getPools();

    List<String> feather = pools.get(1).getNames();
    assertEquals(FeatherIcon.values().length, feather.size());
    assertTrue(feather.contains("arrow-up"));

    List<String> dwc = pools.get(2).getNames();
    assertEquals(DwcIcon.values().length, dwc.size());
    assertTrue(dwc.contains("chevron-down"));
  }
}

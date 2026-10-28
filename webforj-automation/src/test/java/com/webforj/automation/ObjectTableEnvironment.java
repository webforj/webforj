package com.webforj.automation;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.BBjAPI;
import com.basis.bbj.proxies.BBjObjectTable;
import com.webforj.Environment;
import java.util.HashMap;
import java.util.Map;

/** The mocked environment whose object table keeps what is put into it. */
public final class ObjectTableEnvironment {

  private ObjectTableEnvironment() {}

  /** Creates the environment, with an empty object table backed by a map. */
  public static Environment create() throws Exception {
    Map<Object, Object> entries = new HashMap<>();
    Environment environment = mock(Environment.class);
    BBjAPI api = mock(BBjAPI.class);
    BBjObjectTable table = mock(BBjObjectTable.class);
    when(environment.getBBjAPI()).thenReturn(api);
    when(api.getObjectTable()).thenReturn(table);
    when(table.get(any())).thenAnswer(call -> entries.get(call.getArgument(0)));
    doAnswer(call -> entries.put(call.getArgument(0), call.getArgument(1))).when(table).put(any(),
        any());

    return environment;
  }
}

package com.webforj.plugin.maven;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import com.webforj.plugin.foundation.push.PushKeyCommand;
import java.util.ArrayList;
import java.util.List;
import org.apache.maven.plugin.logging.Log;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

class PushKeysMojoTest {

  @Test
  void shouldPrintTheThreeConfigurationLines() {
    Log log = mock(Log.class);
    PushKeysMojo mojo = new PushKeysMojo();
    mojo.setLog(log);

    mojo.execute();

    ArgumentCaptor<CharSequence> captor = ArgumentCaptor.forClass(CharSequence.class);
    verify(log, atLeastOnce()).info(captor.capture());
    List<String> lines = new ArrayList<>();
    captor.getAllValues().forEach(line -> lines.add(line.toString()));

    assertEquals(PushKeyCommand.render().size(), lines.size(), lines.toString());
    assertEquals(PushKeyCommand.BANNER, lines.get(0));
    assertEquals(3, lines.stream().filter(line -> line.startsWith("webforj.push.")).count(),
        lines.toString());
  }
}

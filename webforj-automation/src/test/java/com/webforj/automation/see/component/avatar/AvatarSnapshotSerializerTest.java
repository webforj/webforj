package com.webforj.automation.see.component.avatar;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.automation.see.Snapshot;
import com.webforj.automation.see.SnapshotOptions;
import com.webforj.component.avatar.Avatar;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class AvatarSnapshotSerializerTest {

  @ParameterizedTest
  @ValueSource(booleans = {false, true})
  void shouldPrintTheTextAtBothLevels(boolean browserState) {
    Avatar avatar = new Avatar("Ada Lovelace");

    assertEquals("- Avatar \"Ada Lovelace\" [tooltip=\"Ada Lovelace\"]\n",
        Snapshot.render(avatar, new SnapshotOptions().setBrowserState(browserState)));
  }
}

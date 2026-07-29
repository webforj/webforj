package com.webforj.devtools.craftforj.inspector.contribution.content.avatar;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.avatar.Avatar;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class AvatarInitialsContributionTest {

  private final AvatarInitialsContribution contribution = new AvatarInitialsContribution();

  @Test
  void shouldGet() {
    Avatar avatar = mock(Avatar.class);
    when(avatar.getInitials()).thenReturn("JD");

    var result = contribution.get(avatar);

    assertTrue(result.isPresent());
    assertEquals("Initials", result.get().getName());
    assertEquals(PropertyType.TEXT, result.get().getEditorType());
    assertEquals("JD", result.get().getValue());
  }

  @Test
  void shouldSet() {
    Avatar avatar = mock(Avatar.class);

    boolean success = contribution.set(avatar, "AB");

    assertTrue(success);
    verify(avatar).setInitials("AB");
  }

}

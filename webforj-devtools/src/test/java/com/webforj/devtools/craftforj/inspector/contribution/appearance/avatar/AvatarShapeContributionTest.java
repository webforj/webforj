package com.webforj.devtools.craftforj.inspector.contribution.appearance.avatar;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.avatar.Avatar;
import com.webforj.component.avatar.AvatarShape;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class AvatarShapeContributionTest {

  private final AvatarShapeContribution contribution = new AvatarShapeContribution();

  @Test
  void shouldGet() {
    Avatar avatar = mock(Avatar.class);
    when(avatar.getShape()).thenReturn(AvatarShape.SQUARE);

    var result = contribution.get(avatar);

    assertTrue(result.isPresent());
    assertEquals("Shape", result.get().getName());
    assertEquals(PropertyType.SELECT, result.get().getEditorType());
    assertEquals("com.webforj.component.avatar.AvatarShape.SQUARE", result.get().getValue());
  }

  @Test
  void shouldSet() {
    Avatar avatar = mock(Avatar.class);

    boolean success = contribution.set(avatar, AvatarShape.CIRCLE);

    assertTrue(success);
    verify(avatar).setShape(AvatarShape.CIRCLE);
  }

  @Test
  void shouldGetCorrectEnumClass() {
    Avatar avatar = mock(Avatar.class);
    assertEquals(AvatarShape.class, contribution.findEnumClass(avatar));
  }
}

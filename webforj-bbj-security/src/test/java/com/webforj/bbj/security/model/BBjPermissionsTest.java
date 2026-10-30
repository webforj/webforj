package com.webforj.bbj.security.model;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.basis.api.admin.BBjAdminUserPermissions;
import java.util.Locale;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

class BBjPermissionsTest {
  private final Locale defaultLocale = Locale.getDefault();

  @AfterEach
  void tearDown() {
    Locale.setDefault(defaultLocale);
  }

  @ParameterizedTest
  @EnumSource(BBjPermissions.class)
  void shouldGiveThePermissionNameOfTheBBjAdminApi(BBjPermissions permission) throws Exception {
    Object name = BBjAdminUserPermissions.class.getField(permission.name()).get(null);

    assertEquals(name, permission.toString());
  }

  @Test
  void shouldGiveTheSameNameInEveryDefaultLocale() {
    Locale.setDefault(Locale.forLanguageTag("tr"));

    assertEquals("admin-access", BBjPermissions.ADMIN_ACCESS.toString());
  }
}

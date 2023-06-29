package org.dwcj.component;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.basis.bbj.proxies.sysgui.BBjButton;
import com.basis.startup.type.BBjException;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
public class AbstractDwcComponentTest {
  @Mock
  BBjButton control;

  @InjectMocks
  AbstractDwcComponentMock component;

  @Nested
  @DisplayName("Expanse API")
  class ExpanseApi {

    @ParameterizedTest
    @EnumSource(Expanse.class)
    @DisplayName("Setting/getting expanse")
    void settingGettingExpanse(Expanse expanse) throws BBjException {
      component.setExpanse(expanse);
      assertSame(component.getExpanse(), expanse);

      verify(control, times(1)).putClientProperty("expanse", expanse.getValue());
      verify(control, times(0)).getClientProperty("expanse");
    }
  }
}

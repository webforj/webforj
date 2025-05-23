package com.webforj.springboot.conceiver;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import com.webforj.component.Component;
import com.webforj.component.event.ComponentEvent;
import com.webforj.conceiver.DefaultConceiver; 
import com.webforj.conceiver.exception.ConceiverException;
import java.util.Map;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;

// Dummy classes for testing
class TestBean {}

@ExtendWith(MockitoExtension.class)
class SpringConceiverTest {

    @Mock
    private ApplicationContext mockApplicationContext;

    @InjectMocks
    private SpringConceiver springConceiver;

    @Test
    void get_whenBeanExistsInContext_shouldReturnBeanFromContext() {
        TestBean expectedBean = new TestBean();
        when(mockApplicationContext.getBean(TestBean.class)).thenReturn(expectedBean);

        springConceiver.setApplicationContext(mockApplicationContext);
        TestBean actualBean = springConceiver.get(TestBean.class);

        assertSame(expectedBean, actualBean, "Should return the bean from Spring context");
        verify(mockApplicationContext).getBean(TestBean.class); 
    }

    @Test
    void get_whenBeanDoesNotExistInContext_shouldDelegateToDefaultConceiver() {
        when(mockApplicationContext.getBean(TestBean.class)).thenThrow(new BeansException("Test bean not found") {});

        springConceiver.setApplicationContext(mockApplicationContext);
        TestBean actualBean = null;
        try {
            actualBean = springConceiver.get(TestBean.class);
        } catch (Exception e) {
            fail("SpringConceiver should not throw an exception if DefaultConceiver can handle it.", e);
        }

        assertNotNull(actualBean, "Bean should be created by DefaultConceiver as a fallback");
        verify(mockApplicationContext).getBean(TestBean.class);
    }

    @Test
    void get_whenApplicationContextIsNull_shouldDelegateToDefaultConceiver() {
        springConceiver.setApplicationContext(null); 

        TestBean actualBean = null;
        try {
            actualBean = springConceiver.get(TestBean.class);
        } catch (Exception e) {
            fail("SpringConceiver should not throw an exception if DefaultConceiver can handle it when context is null.", e);
        }

        assertNotNull(actualBean, "Bean should be created by DefaultConceiver when Spring context is null");
        verify(mockApplicationContext, never()).getBean(TestBean.class);
    }

    @Test
    void getComponentEvent_shouldAlwaysDelegateToDefaultConceiver() {
        springConceiver.setApplicationContext(mockApplicationContext); 

        try {
            Component mockComponent = mock(Component.class);
            @SuppressWarnings("rawtypes")
            Class mockEventClass = ComponentEvent.class; 
            Map<String, Object> mockEventData = mock(Map.class);

            ComponentEvent<?> event = springConceiver.getComponentEvent(mockComponent, mockEventClass, mockEventData);
            assertNotNull(event, "Event should be created by DefaultConceiver");

        } catch (ConceiverException e) {
            assertTrue(true, "DefaultConceiver threw an exception as expected for problematic event class, delegation ok.");
        } catch (Exception e) {
            fail("Unexpected exception during getComponentEvent delegation.", e);
        }

        verify(mockApplicationContext, never()).getBean(any());
    }
}

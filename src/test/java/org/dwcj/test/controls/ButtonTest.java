package org.dwcj.test.controls;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class ButtonTest{

    private ButtonTestApp testApp;

    @BeforeEach
    void buttonCreateTest(){
        testApp = new ButtonTestApp();
    }

    @AfterEach
    void buttonTerminateTest(){
        testApp.terminate();
    }

    @Test
    void getButtonTextTest(){
        assertEquals("test",testApp.getTestButton().getText());
    }

    @Test
    void buttonClickTest(){
        testApp.registerButtonClickEvent();
        testApp.performButtonClick();
        assertTrue(testApp.isButtonClicked());
    }

}

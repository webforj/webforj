package org.dwcj.test.selenium;

import io.github.bonigarcia.wdm.WebDriverManager;
import org.openqa.selenium.By;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.chrome.ChromeDriver;

import org.junit.jupiter.api.Test;

import java.time.Duration;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class SeleniumButtonTest {

    @Test
    void testButtonPush(){
        System.setProperty("webdriver.chrome.driver","/home/beff/chromedriver");
        WebDriverManager.chromedriver().setup();
        ChromeDriver driver = new ChromeDriver();

        driver.get("http://localhost:8888/webapp/dwcj?class=dwcjsample.ButtonPushSample");

        driver.manage().timeouts().implicitlyWait(Duration.ofMillis(500));

        WebElement button = driver.findElement(By.id("bbj-button-9"));


        WebElement output = driver.findElement((By.id("10")));

        String outputtext = output.getAttribute("innerHTML");
        assertEquals("not pushed",outputtext);

        String buttontext = output.getAttribute("innerHTML");
        assertEquals("not pushed",buttontext);

        button.click();
        driver.manage().timeouts().implicitlyWait(Duration.ofMillis(500));

        outputtext = output.getAttribute("innerHTML");
        assertEquals("pushed",outputtext);

        buttontext = output.getAttribute("innerHTML");
        assertEquals("pushed",buttontext);

        driver.close();
        driver.quit();
    }

}

package com.webforj.kotlin.dsl.component.login

import com.webforj.component.html.elements.Div
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.component.button.button
import com.webforj.kotlin.dsl.component.text.label
import org.junit.jupiter.api.*
import org.junit.jupiter.api.Assertions.assertTrue
import kotlin.test.assertEquals
import kotlin.test.assertNotNull

class LoginTest {
    lateinit var root: HasComponents

    @BeforeEach
    fun setUp() {
        root = Div()
    }

    @AfterEach
    fun tearDown() {
        root.removeAll()
    }

    @Test
    @DisplayName("Create empty Login")
    fun shouldCreateEmptyLogin() {
        val login = root.login()
        assertNotNull(login)
        assertTrue(root.hasComponent(login))
    }

    @Test
    @DisplayName("Create Login with before header components")
    fun shouldCreateLoginWithBeforeHeaderComponents() {
        val login = root.login {
            beforeHeader {
                val welcomeLabel = label("Welcome Back")
                val helpButton = button("Help")
                assertEquals("Welcome Back", welcomeLabel.text)
                assertEquals("Help", helpButton.text)
            }
        }

        assertTrue(root.hasComponent(login))
    }

    @Test
    @DisplayName("Create Login with after header components")
    fun shouldCreateLoginWithAfterHeaderComponents() {
        val login = root.login {
            afterHeader {
                val assistanceLabel = label("Need assistance?")
                val supportButton = button("Contact Support")
                assertEquals("Need assistance?", assistanceLabel.text)
                assertEquals("Contact Support", supportButton.text)
            }
        }

        assertTrue(root.hasComponent(login))
    }

    @Test
    @DisplayName("Create Login with before content components")
    fun shouldCreateLoginWithBeforeContentComponents() {
        val login = root.login {
            beforeContent {
                val credentialsLabel = label("Please enter your credentials")
                val tourButton = button("Tour")
                assertEquals("Please enter your credentials", credentialsLabel.text)
                assertEquals("Tour", tourButton.text)
            }
        }

        assertTrue(root.hasComponent(login))
    }

    @Test
    @DisplayName("Create Login with after content components")
    fun shouldCreateLoginWithAfterContentComponents() {
        val login = root.login {
            afterContent {
                val signUpLabel = label("New user? Sign up")
                val createButton = button("Create Account")
                assertEquals("New user? Sign up", signUpLabel.text)
                assertEquals("Create Account", createButton.text)
            }
        }

        assertTrue(root.hasComponent(login))
    }

    @Test
    @DisplayName("Create Login with before form components")
    fun shouldCreateLoginWithBeforeFormComponents() {
        val login = root.login {
            beforeForm {
                val detailsLabel = label("Enter your details below")
                val infoButton = button("Info")
                assertEquals("Enter your details below", detailsLabel.text)
                assertEquals("Info", infoButton.text)
            }
        }

        assertTrue(root.hasComponent(login))
    }

    @Test
    @DisplayName("Create Login with after form components")
    fun shouldCreateLoginWithAfterFormComponents() {
        val login = root.login {
            afterForm {
                val forgotLabel = label("Forgot your password?")
                val resetButton = button("Reset")
                assertEquals("Forgot your password?", forgotLabel.text)
                assertEquals("Reset", resetButton.text)
            }
        }

        assertTrue(root.hasComponent(login))
    }

    @Test
    @DisplayName("Create Login with before footer components")
    fun shouldCreateLoginWithBeforeFooterComponents() {
        val login = root.login {
            beforeFooter {
                val copyrightLabel = label("© 2026 My Company")
                val termsButton = button("Terms")
                assertEquals("© 2026 My Company", copyrightLabel.text)
                assertEquals("Terms", termsButton.text)
            }
        }

        assertTrue(root.hasComponent(login))
    }

    @Test
    @DisplayName("Create Login with after footer components")
    fun shouldCreateLoginWithAfterFooterComponents() {
        val login = root.login {
            afterFooter {
                val privacyLabel = label("Privacy Policy")
                val contactButton = button("Contact")
                assertEquals("Privacy Policy", privacyLabel.text)
                assertEquals("Contact", contactButton.text)
            }
        }

        assertTrue(root.hasComponent(login))
    }

    @Test
    @DisplayName("Create Login with loginI18n configuration")
    fun shouldCreateLoginWithI18nConfiguration() {
        val login = root.login {
            loginI18n {
                title = "Sign In"
                username = "Email Address"
                password = "Password"
                submit = "Log In"
            }
        }

        assertTrue(root.hasComponent(login))
        val i18n = login.i18n
        assertNotNull(i18n)
        assertEquals("Sign In", i18n.title)
        assertEquals("Email Address", i18n.username)
        assertEquals("Password", i18n.password)
        assertEquals("Log In", i18n.submit)
    }

    @Test
    @DisplayName("Create Login with loginErrorI18n configuration")
    fun shouldCreateLoginWithErrorI18nConfiguration() {
        val login = root.login {
            loginI18n {
                loginErrorI18n {
                    title = "Invalid username or password"
                    message = "Account is temporarily locked"
                }
            }
        }

        assertTrue(root.hasComponent(login))
        val i18n = login.i18n
        assertNotNull(i18n)
        val errorI18n = i18n.error
        assertNotNull(errorI18n)
        assertEquals("Invalid username or password", errorI18n.title)
        assertEquals("Account is temporarily locked", errorI18n.message)
    }

    @Test
    @DisplayName("Create comprehensive Login with all slots configured")
    fun shouldCreateComprehensiveLogin() {
        val login = root.login {
            beforeHeader {
                val welcomeLabel = label("Welcome Back")
                assertEquals("Welcome Back", welcomeLabel.text)
            }
            afterHeader {
                val helpButton = button("Help")
                assertEquals("Help", helpButton.text)
            }
            beforeContent {
                val credentialsLabel = label("Please enter your credentials")
                assertEquals("Please enter your credentials", credentialsLabel.text)
            }
            afterContent {
                val signUpLabel = label("New user? Sign up")
                assertEquals("New user? Sign up", signUpLabel.text)
            }
            beforeForm {
                val detailsLabel = label("Enter your details below")
                assertEquals("Enter your details below", detailsLabel.text)
            }
            afterForm {
                val forgotButton = button("Forgot Password")
                assertEquals("Forgot Password", forgotButton.text)
            }
            beforeFooter {
                val copyrightLabel = label("© 2026 My Company")
                assertEquals("© 2026 My Company", copyrightLabel.text)
            }
            afterFooter {
                val privacyButton = button("Privacy Policy")
                assertEquals("Privacy Policy", privacyButton.text)
            }
            loginI18n {
                title = "Sign In"
                username = "Email Address"
                password = "Password"
                submit = "Log In"
                loginErrorI18n {
                    title = "Invalid username or password"
                    message = "Account is temporarily locked"
                }
            }
        }

        assertTrue(root.hasComponent(login))

        val i18n = login.i18n
        assertNotNull(i18n)
        assertEquals("Sign In", i18n.title)
        assertEquals("Email Address", i18n.username)
        assertEquals("Password", i18n.password)
        assertEquals("Log In", i18n.submit)
        
        val errorI18n = i18n.error
        assertNotNull(errorI18n)
        assertEquals("Invalid username or password", errorI18n.title)
        assertEquals("Account is temporarily locked", errorI18n.message)
    }

}
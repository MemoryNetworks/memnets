package memnets.models

import memnets.core.Topic
import memnets.model._

object CommonTopics {
  val swtaTopic = Topic(
    """Winner-take-all is a computational principle applied in computational models of neural networks by which neurons in a layer compete with each other for activation.
      |
      |In the theory of artificial neural networks, winner-take-all networks are a case of competitive learning in recurrent neural networks.
      |
      |Output nodes in the network mutually inhibit each other, while simultaneously activating themselves through reflexive connections.
      |
      |After some time, only one node in the output layer will be active, namely the one corresponding to the strongest input.
      |
      |Thus the network uses nonlinear inhibition to pick out the largest of a set of inputs.
      |
      |Soft winner take-all allows more than one neuron to be active.
      |""".stripMargin,
    "https://en.wikipedia.org/wiki/Winner-take-all_(computing)"
  )

  val phaseSyncTopic = Topic(
    """Phase synchronization is the process by which two or more cyclic signals tend to oscillate with a repeating sequence of relative phase angles.
      |
      |Phase synchronisation is usually applied to two waveforms of the same frequency with identical phase angles with each cycle. However it can be applied if there is an integer relationship of frequency, such that the cyclic signals share a repeating sequence of phase angles over consecutive cycles. These integer relationships are called Arnold tongues which follow from bifurcation of the circle map.
      |
      |One example of phase synchronization of multiple oscillators can be seen in the behavior of Southeast Asian fireflies. At dusk, the flies begin to flash periodically with random phases and a gaussian distribution of native frequencies. As night falls, the flies, sensitive to one another's behavior, begin to synchronize their flashing. After some time all the fireflies within a given tree (or even larger area) will begin to flash simultaneously in a burst.
      |
      |Thinking of the fireflies as biological oscillators, we can define the phase to be 0° during the flash and +-180° exactly halfway until the next flash. Thus, when they begin to flash in unison, they synchronize in phase.
      |
      |One way to keep a local oscillator "phase synchronized" with a remote transmitter uses a phase-locked loop.
      |""".stripMargin,
    "https://en.wikipedia.org/wiki/Phase_synchronization"
  )

  val coupledOscTopic = Topic(
    """The harmonic oscillator and the systems it models have a single degree of freedom.
      |
      |More complicated systems have more degrees of freedom, for example two masses and three springs (each mass being attached to fixed points and to each other).
      |
      |In such cases, the behavior of each variable influences that of the others.  This leads to a coupling of the oscillations of the individual degrees of freedom.
      |
      |For example, two pendulum clocks (of identical frequency) mounted on a common wall will tend to synchronise.  This phenomenon was first observed by Christiaan Huygens in 1665.
      |
      |The apparent motions of the compound oscillations typically appears very complicated but a more economic, computationally simpler and conceptually deeper description is given by resolving the motion into normal modes.
      |
      |More special cases are the coupled oscillators where energy alternates between two forms of oscillation.
      |
      |Well-known is the Wilberforce pendulum, where the oscillation alternates between an elongation of a vertical spring and the rotation of an object at the end of that spring.
      |
      |Coupled oscillators is a common description of two related, but different phenomena.
      |
      |One case is where both oscillations affect each other mutually, which usually leads to the occurrence of a single, entrained oscillation state, where both oscillate with a compromise frequency.
      |
      |Another case is where one external oscillation affects an internal oscillation, but is not affected by this.  In this case there are regions of synchronization, known as Arnold tongues.
      |
      |The latter case can lead to highly complex phenomena as for instance chaotic dynamics.
      |""".stripMargin,
    "https://en.wikipedia.org/wiki/Oscillation#Coupled_oscillations"
  )

  val heatTopic = Topic(
    """In physics and mathematics, the heat equation is a partial differential equation that describes how the distribution of some quantity (such as heat) evolves over time in a solid medium, as it spontaneously flows from places where it is higher towards places where it is lower.
      |
      |It is a special case of the diffusion equation.
      |
      |This equation was first developed and solved by Joseph Fourier in 1822 to describe heat flow.
      |
      |However, it is of fundamental importance in diverse scientific fields.
      |
      |In probability theory, the heat equation is connected with the study of random walks and Brownian motion, via the Fokker–Planck equation.
      |
      |In financial mathematics it is used to solve the Black–Scholes partial differential equation.
      |
      |A variant was also instrumental in the solution of the longstanding Poincaré conjecture of topology.
      |""".stripMargin,
    "https://en.wikipedia.org/wiki/Heat_equation"
  )

  val pageRankTopic = Topic(
    """PageRank (PR) is an algorithm used by Google Search to rank web pages in their search engine results.
      |
      |PageRank was named after Larry Page, one of the founders of Google.
      |
      |PageRank is a way of measuring the importance of website pages. According to Google:
      |
      |    PageRank works by counting the number and quality of links to a page to determine a rough estimate of how important the website is.
      |    The underlying assumption is that more important websites are likely to receive more links from other websites.
      |
      |Currently, PageRank is not the only algorithm used by Google to order search results, but it is the first algorithm that was used by the company, and it is the best known
      |""".stripMargin,
    "https://en.wikipedia.org/wiki/PageRank"
  )

  val resonance = Topic(
    """In mechanical systems, resonance is a phenomenon that only occurs when the frequency at which a force is periodically applied is equal or nearly equal to one of the natural frequencies of the system on which it acts.
      |
      |This causes the system to oscillate with larger amplitude than when the force is applied at other frequencies.
      |
      |Frequencies at which the response amplitude is a relative maximum are also known as resonant frequencies or resonance frequencies of the system.
      |
      |Near resonant frequencies, small periodic forces have the ability to produce large amplitude oscillations, due to the storage of vibrational energy.
      |
      |In other systems, such as electrical or optical, phenomena occur which are described as resonance but depend on the interaction between different aspects of the system, not on an external driver.
      |""".stripMargin,
    "https://en.wikipedia.org/wiki/Resonance"
  )

  val fourierTranform = Topic(
    """The Fourier transform (FT) decomposes a function of time (a signal) into its constituent frequencies.
      |
      |This is similar to the way a musical chord can be expressed in terms of the volumes and frequencies of its constituent notes.
      |
      |The term Fourier transform refers to both the frequency domain representation and the mathematical operation that associates the frequency domain representation to a function of time.
      |
      |The Fourier transform of a function of time is itself a complex-valued function of frequency, whose magnitude (modulus) represents the amount of that frequency present in the original function, and whose argument is the phase offset of the basic sinusoid in that frequency.
      |
      |The Fourier transform is not limited to functions of time, but the domain of the original function is commonly referred to as the time domain.
      |
      |There is also an inverse Fourier transform that mathematically synthesizes the original function from its frequency domain representation.
      |""".stripMargin,
    "https://en.wikipedia.org/wiki/Fourier_transform"
  )

  val harmonicOscillator = Topic(
    """In classical mechanics, a harmonic oscillator is a system that, when displaced from its equilibrium position, experiences a restoring force F proportional to the displacement x:
      |
      |    F = − k x
      |
      |where k is a positive constant.
      |
      |If F is the only force acting on the system, the system is called a simple harmonic oscillator, and it undergoes simple harmonic motion: sinusoidal oscillations about the equilibrium point, with a constant amplitude and a constant frequency (which does not depend on the amplitude).
      |
      |If a frictional force (damping) proportional to the velocity is also present, the harmonic oscillator is described as a damped oscillator. Depending on the friction coefficient, the system can:
      |
      |    Oscillate with a frequency lower than in the undamped case, and an amplitude decreasing with time (underdamped oscillator).
      |    Decay to the equilibrium position, without oscillations (overdamped oscillator).
      |
      |The boundary solution between an underdamped oscillator and an overdamped oscillator occurs at a particular value of the friction coefficient and is called critically damped.
      |
      |If an external time-dependent force is present, the harmonic oscillator is described as a driven oscillator.
      |
      |Mechanical examples include pendulums (with small angles of displacement), masses connected to springs, and acoustical systems.
      |Other analogous systems include electrical harmonic oscillators such as RLC circuits.
      |The harmonic oscillator model is very important in physics, because any mass subject to a force in stable equilibrium acts as a harmonic oscillator for small vibrations.
      |Harmonic oscillators occur widely in nature and are exploited in many manmade devices, such as clocks and radio circuits. They are the source of virtually all sinusoidal vibrations and waves.
      |""".stripMargin,
    "https://en.wikipedia.org/wiki/Harmonic_oscillator"
  )
}

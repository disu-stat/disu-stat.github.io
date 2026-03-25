import { useEffect, useState } from "react";

export default function AcademicSinglePageSite() {
  const [openAbstract, setOpenAbstract] = useState<Record<string, boolean>>({});
  const [isNameHovered, setIsNameHovered] = useState(false);
  const [activeSection, setActiveSection] = useState("home");
  const [lockedSection, setLockedSection] = useState<string | null>(null);

  const sections = [
    { id: "home", label: "Home" },
    { id: "research", label: "Research" },
    { id: "talks", label: "Talks" },
    { id: "teaching", label: "Teaching" },
    { id: "awards", label: "Awards" },
  ];

  const accent = "#7C3AED";
  const accentSoft = "#f9f7fc";
  const accentBorder = "#A1A1AA";
  const accentMuted = "#6D28D9";

  // update tab title (default)
  useEffect(() => {
    document.title = "Di Su";
  }, []);

  // update tab title on hover
  useEffect(() => {
    document.title = isNameHovered ? "肃棣" : "Di Su";
  }, [isNameHovered]);

  const handleSectionActivate = (id: string) => {
    setActiveSection(id);
  };

  useEffect(() => {
    const onScroll = () => {
      if (lockedSection) return;

      // Fix: ensure Home is active when near top
      if (window.scrollY < 120) {
        setActiveSection("home");
        return;
      }

      const anchor = window.innerHeight * 0.28;
      let current = sections[0].id;

      for (const section of sections) {
        const el = document.getElementById(section.id);
        if (!el) continue;
        if (el.getBoundingClientRect().top <= anchor) current = section.id;
      }

      setActiveSection(current);
    };

    onScroll();
    window.addEventListener("scroll", onScroll, { passive: true });
    window.addEventListener("resize", onScroll);

    return () => {
      window.removeEventListener("scroll", onScroll);
      window.removeEventListener("resize", onScroll);
    };
  }, [lockedSection]);

useEffect(() => {
  const links = document.querySelectorAll("a[href^='http']");
  links.forEach((link) => {
    link.setAttribute("target", "_blank");
    link.setAttribute("rel", "noreferrer");
  });
}, []);
  
  const scrollToSection = (id: string) => {
    handleSectionActivate(id);
    setLockedSection(id);
    document.getElementById(id)?.scrollIntoView({ behavior: "smooth", block: "start" });

    window.setTimeout(() => {
      setLockedSection(null);
    }, 700);
  };

  const Section = ({
    id,
    title,
    children,
  }: {
    id: string;
    title: string;
    children: React.ReactNode;
  }) => (
    <section id={id} className="scroll-mt-20 py-8">
      <h2 style={{ color: accentMuted }} className="mb-3 text-[1.6rem] font-semibold">
        {title}
      </h2>
      <div className="space-y-2 text-[15px] text-zinc-700">{children}</div>
    </section>
  );

  const Entry = ({
    id,
    title,
    authors,
    venue,
    links = [],
    abstract,
  }: {
    id: string;
    title: string;
    authors: string;
    venue: string;
    links?: Array<{ label: string; href: string }>;
    abstract?: React.ReactNode;
  }) => {
    const isOpen = !!openAbstract[id];

    return (
      <div className="py-2 first:pt-0">
        <div className="flex items-start gap-3">
          <span className="mt-[0rem] text-[1rem] leading-none text-zinc-700">•</span>
          <div className="min-w-0 flex-1">
            <div className="text-[16px] font-semibold leading-4 text-zinc-900">{title}</div>
            <div className="mt-0.5 text-[14px] leading-4 text-zinc-700">{authors}</div>
            <div className="mt-0.5 flex flex-wrap items-center gap-1.5 text-[14px] leading-4 text-zinc-500">
              <span>{venue}</span>
              {links.length > 0 && <span className="text-zinc-300">|</span>}
              {links.map((link, index) => (
                <span key={`${id}-${link.label}`} className="flex items-center gap-1.5">
                  <a
                    href={link.href}
                    target="_blank"
                    rel="noreferrer"
                    className="text-[14px] text-[color:var(--accent)]/80 hover:text-[color:var(--accent)]"
                  >
                    {link.label}
                  </a>
                  {index < links.length - 1 && <span className="text-zinc-300">|</span>}
                </span>
              ))}
              {abstract ? (
                <>
                  {(links.length > 0 || venue) && <span className="text-zinc-300">|</span>}
                  <button
                    type="button"
                    onClick={() => setOpenAbstract((prev) => ({ ...prev, [id]: !prev[id] }))}
                    className="text-[14px] text-[color:var(--accent)]/80 hover:text-[color:var(--accent)]"
                  >
                    {isOpen ? "Hide abstract" : "Abstract"}
                  </button>
                </>
              ) : null}
            </div>
            {abstract && isOpen && (
              <div
                className="mt-2 rounded-md px-3 py-2 text-[14px] leading-6 text-zinc-700"
                style={{ background: accentSoft }}
              >
                {abstract}
              </div>
            )}
          </div>
        </div>
      </div>
    );
  };

  const TalkEntry = ({
    info,
    title,
    slidesHref,
  }: {
    info: string;
    title: string;
    slidesHref?: string;
  }) => {
    return (
      <div className="py-2.5 first:pt-0">
        <div className="flex items-start gap-3">
          <span className="mt-[0rem] text-[1rem] leading-none text-zinc-700">•</span>
          <div className="flex-1">
            <div className="text-[15px] font-semibold leading-4 text-zinc-900">{info}</div>
            <div className="mt-0.5 flex flex-wrap items-center gap-2 text-[15px] leading-4 text-zinc-700">
              <span>{title}</span>
              {slidesHref ? (
                <>
                  <span className="text-zinc-300">|</span>
                  <a
                    href={slidesHref}
                    target="_blank"
                    rel="noreferrer"
                    className="text-[14px] text-[color:var(--accent)]/80 hover:text-[color:var(--accent)]"
                  >
                    Slides
                  </a>
                </>
              ) : null}
            </div>
          </div>
        </div>
      </div>
    );
  };

  return (
    <div className="min-h-screen bg-white text-zinc-900" style={{ "--accent": accent } as React.CSSProperties}>
      <div className="mx-auto grid max-w-5xl grid-cols-1 gap-10 px-6 py-8 md:grid-cols-[220px_1fr] lg:grid-cols-[240px_1fr]">
        <aside className="relative pr-8 md:sticky md:top-6 md:self-start md:border-r md:border-[rgba(124,58,237,0.2)]">
          <div className="text-[1.35rem] font-bold text-zinc-900">Di Su</div>
          <div className="mt-1 text-[16px] text-zinc-500">PhD candidate @ LSE Statistics</div>

          <div className="mt-3 flex flex-wrap gap-2.5 text-[15px]">
            <a href="mailto:d.su1@lse.ac.uk" className="text-[color:var(--accent)] hover:underline">
              Email
            </a>
            <span className="text-zinc-300">|</span>
            <a href="#" className="text-[color:var(--accent)] hover:underline">
              Scholar
            </a>
            <span className="text-zinc-300">|</span>
            <a href="https://github.com/disu-stat" className="text-[color:var(--accent)] hover:underline">
              GitHub
            </a>
          </div>

          <nav className="mt-7 space-y-2.5 text-[16px]">
            {sections.map((section) => (
              <button
                key={section.id}
                type="button"
                onClick={() => scrollToSection(section.id)}
                className="relative block w-full cursor-pointer text-left hover:text-zinc-900"
                style={{
                  color: activeSection === section.id ? accentMuted : "#555",
                  paddingLeft: activeSection === section.id ? "0.9rem" : "0rem",
                  fontWeight: activeSection === section.id ? 600 : 400,
                }}
              >
                <span
                  className="absolute left-0 top-1/2 h-[1.1rem] w-[2px] -translate-y-1/2 rounded-full"
                  style={{ background: activeSection === section.id ? accentMuted : "transparent" }}
                />
                {section.label}
              </button>
            ))}
          </nav>
        </aside>

        <main>
          <section
            id="home"
            className="grid max-w-3xl grid-cols-1 items-start gap-5 scroll-mt-20 sm:grid-cols-[1fr_150px] sm:gap-8"
          >
            <div>
              <h1
                className="text-[2.2rem] font-bold text-zinc-950"
                onMouseEnter={() => setIsNameHovered(true)}
                onMouseLeave={() => setIsNameHovered(false)}
              >
                <span
                  style={
                    isNameHovered
                      ? {
                          fontFamily: '"STSong", "Songti SC", "Noto Serif SC", serif',
                          letterSpacing: "0.08em",
                          fontWeight: 300,
                        }
                      : undefined
                  }
                >
                  {isNameHovered ? "肃棣" : "Di Su"}
                </span>
              </h1>

              <p className="mt-3 max-w-2xl text-[15px] leading-7 text-zinc-700">
                I am a PhD candidate in Statistics at the London School of Economics, working with <a href="https://personal.lse.ac.uk/wangt60/">Tengyao Wang</a> and <a href="https://personal.lse.ac.uk/cheny100/">Yining Chen</a>. My research lies in change-region detection and statistical learning. I hold an MPhil in Statistics and a BSc in Mathematics, both earned at The Chinese University of Hong Kong (CUHK).
              </p>
            </div>

            <div className="order-first sm:order-none">
              <div className="h-28 w-28 overflow-hidden rounded-sm bg-zinc-100 sm:ml-auto sm:h-36 sm:w-36">
                <img
                  src="src/assets/DSC04178.JPG"
                  alt="Di Su"
                  className="h-full w-full object-cover"
                />
              </div>
            </div>
          </section>

          <Section id="research" title="Research">
            <Entry
              id="p1"
              title="Detecting change regions on spheres"
              authors="Di Su, Yining Chen, Tengyao Wang"
              venue="Preprint"
              links={[{ label: "PDF", href: "https://arxiv.org/pdf/2603.22071" }]}
              abstract={
                <>
                  While change point detection in time series data has been extensively studied, little attention has been given to its generalisation to data observed on spheres or other manifolds, where changes may occur within spatially complex regions with irregular boundaries, posing significant challenges. We propose a new class of estimators, namely, Change Region Identification and SeParation (CRISP), to locate changes in the mean function of a signalplus-noise model defined on 𝑑-dimensional spheres. The CRISP estimator applies to scenarios with a single change region, and is extended to multiple change regions via a newly developed generic scheme. The convergence rate of the CRISP estimator is shown to depend on the VC dimension of the hypothesis class that characterises the change regions in general. We also carefully study the case where change regions have the geometry of spherical caps. Simulations confirm the promising finite-sample performance of this approach. The CRISP estimator’s practical applicability is further demonstrated through two real data sets on global temperature and ozone hole.
                </>
              }
            />
          </Section>

          <Section id="talks" title="Talks">
            <TalkEntry
              info="Invited talk, RSS International Conference (Royal Statistical Society), Bournemouth, Sept. 2026"
              title="Detecting change regions on spheres"
            />
            <TalkEntry
              info="Invited talk, The 8th International Conference on Econometrics and Statistics (Waseda University), Tokyo, Aug. 2025"
              title="Detecting change regions on spheres"
            />
            <TalkEntry
              info="Invited talk, Statistical Modeling with Applications (Belgrade University), Belgrade, Sept. 2024"
              title="Detecting change regions on spheres"
            />
            <TalkEntry
              info="Invited talk, The 12th ICSA International Conference (Chinese University of Hong Kong), Hong Kong, Jul. 2023"
              title="Variance Estimation of Spatial Autocorrelated Data under Non-constant Mean"
            />
          </Section>

          <Section id="teaching" title="Teaching">
            <div className="py-2 first:pt-0">
              <div className="text-[15px] font-semibold leading-6 text-zinc-900">GTA @ LSE</div>
              <div className="mt-0.5 text-[14px] leading-4 text-zinc-700">ST107 Quantitative Methods (2024, 2025, 2026)</div>
              <div className="mt-0.5 text-[14px] leading-4 text-zinc-700">ST102 Elementary Statistical Theory (2026)</div>
            </div>
            <div className="py-2 first:pt-0">
              <div className="text-[15px] font-semibold leading-6 text-zinc-900">TA @ CUHK</div>
              <div className="mt-0.5 text-[14px] leading-4 text-zinc-700">STAT5104 Data Mining (2023)</div>
              <div className="mt-0.5 text-[14px] leading-4 text-zinc-700">STAT4010 Bayesian Learning (2022, 2023)</div>
              <div className="mt-0.5 text-[14px] leading-4 text-zinc-700">STAT1011 Introduction to Statistics (2021, 2022)</div>
              <div className="mt-0.5 text-[14px] leading-4 text-zinc-700">Fundamentals of Deep Learning Workshop (2023)</div>
            </div>
          </Section>

          <Section id="awards" title="Awards">
            <div className="space-y-2">
              <div className="mt-0.5 text-[15px] leading-4 text-zinc-700">LSE PhD Studentship (2024, 2025, 2026, 2027)</div>
              <div className="mt-0.5 text-[15px] leading-4 text-zinc-700">Poster Presentation Award, Second Runner-up, CUHK Science Faculty Postgraduate Research Day (2023)</div>
              <div className="mt-0.5 text-[15px] leading-4 text-zinc-700">Postgraduate Studentship, CUHK (2022, 2023)</div>
              <div className="mt-0.5 text-[15px] leading-4 text-zinc-700">Head’s List (Merit), CUHK (2021)</div>
              <div className="mt-0.5 text-[15px] leading-4 text-zinc-700">Dean’s List, CUHK (2020, 2021)</div>
              <div className="mt-0.5 text-[15px] leading-4 text-zinc-700">Tuition Fee Scholarship, CUHK (2017, 2018, 2019, 2020)</div>
              <div className="mt-0.5 text-[15px] leading-4 text-zinc-700">University Mathematics Scholarship, CUHK (2020)</div>
              <div className="mt-0.5 text-[15px] leading-4 text-zinc-700">Bronze Prize, China Girls Mathematical Olympiad (2015)</div>
            </div>
          </Section>
        </main>
      </div>
    </div>
  );
}

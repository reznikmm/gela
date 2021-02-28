# SPDX-FileCopyrightText: 2021 Max Reznik <reznikmm@gmail.com>
#
# SPDX-License-Identifier: MIT
#

%undefine _hardened_build
%define _gprdir %_GNAT_project_dir
%define rtl_version 0.1

Name:       gela
Version:    0.4.0
Release:    git%{?dist}
Summary:    Ada code analyzer library
Group:      Development/Libraries
License:    MIT
URL:        https://github.com/reznikmm/gela
### Direct download is not availeble
Source0:    gela.tar.gz
BuildRequires:   gcc-gnat
BuildRequires:   fedora-gnat-project-common  >= 3 
BuildRequires:   matreshka-devel
BuildRequires:   anagram-devel
BuildRequires:   gprbuild

# gprbuild only available on these:
ExclusiveArch: %GPRbuild_arches

%description
The gela is a library to analyze Ada sources.

%package devel

Group:      Development/Libraries
License:    MIT
Summary:    Devel package for the gela
Requires:       %{name}%{?_isa} = %{version}-%{release}
Requires:   fedora-gnat-project-common  >= 2

%description devel
Devel package for gela

%prep 
%setup -q -n %{name}

%build
# Disable LTO
%define _lto_cflags %{nil}
make  %{?_smp_mflags} GPRBUILD_FLAGS="%Gnatmake_optflags"

%install
rm -rf %{buildroot}
make install DESTDIR=%{buildroot} LIBDIR=%{_libdir} PREFIX=%{_prefix} GPRDIR=%{_gprdir} BINDIR=%{_bindir}

%check
## find libs without RPATH, Fedora specific
export LD_LIBRARY_PATH="%{buildroot}/%{_libdir}/:$LD_LIBRARY_PATH"
make  %{?_smp_mflags} GPRBUILD_FLAGS="%Gnatmake_optflags" check

%post     -p /sbin/ldconfig
%postun   -p /sbin/ldconfig

%files
%doc LICENSES/*
%dir %{_libdir}/oasis
%{_libdir}/oasis/liboasis.so.%{rtl_version}
%{_libdir}/liboasis.so.%{rtl_version}
%{_libdir}/oasis/liboasis.so.0
%{_libdir}/liboasis.so.0
%{_libdir}/oasis/liboasis_pl.so.%{rtl_version}
%{_libdir}/liboasis_pl.so.%{rtl_version}
%{_libdir}/oasis/liboasis_pl.so.0
%{_libdir}/liboasis_pl.so.0
%files devel
%doc README.md
%{_libdir}/oasis/liboasis.so
%{_libdir}/liboasis.so
%{_libdir}/oasis/liboasis_pl.so
%{_libdir}/liboasis_pl.so
%{_libdir}/oasis/*.ali
%{_includedir}/oasis
%{_gprdir}/oasis.gpr
%{_gprdir}/manifests/oasis
%{_gprdir}/oasis_plain.gpr
%{_gprdir}/manifests/oasis_plain

%changelog
* Sun Feb 28 2021 Maxim Reznik <reznikmm@gmail.com> - 0.1.0-git
- Initial package

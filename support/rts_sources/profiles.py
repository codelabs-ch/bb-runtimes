#
# Copyright (C) 2016-2018, AdaCore
#
# Generates the scenario variables values to bind the rts sources with the
# BSP to actually create a runtime project.

import sys
from support.rts_sources import Rule
from support.rts_sources.sources import all_scenarios, sources


class RTSProfiles(object):
    """Defines the scenarios in the shared rts projects"""

    def __init__(self, config):
        """class used to generate the base RTS profiles to be used by BSPs as a
        basis for the various runtimes.

        :type config: an instance of
        :type sourcetree: an instance of rts_sources.ProjectTree
        """
        # config is a bsp_sources.Target object that defines the properties
        # that are necessary to configure the runtime sources.
        self.config = config

    def check_deps(self, scenarios):
        while True:
            modified = False
            for d, content in sources.items():
                matches = False
                if 'requires' not in content:
                    continue
                if 'conditions' not in content:
                    matches = True
                else:
                    rule = Rule(content['conditions'], all_scenarios)
                    if rule.matches(scenarios):
                        matches = True
                if matches:
                    dep = Rule(content['requires'], all_scenarios)
                    if not dep.matches(scenarios):
                        modified = True
                        scenarios.update(dep.corresponding_scenario())
            if not modified:
                break

    def zfp_scenarios(self, profile='zfp'):
        """Returns the list of directories contained in a base ZFP runtime"""
        ret = {}
        ret['RTS_Profile'] = profile
        ret['Add_Arith64'] = "yes"
        ret['Add_Case_Util:yes'] = "yes"
        ret['Add_Exponent_Float'] = "yes"
        ret['Add_Exponent_Int'] = "yes"
        ret['Add_Exponent_LL_Int'] = "yes"
        ret['Add_Exponent_Modular'] = "yes"
        ret['Add_Float_Util'] = "yes"

        ret['Add_Image_Enum'] = 'yes'
        ret['Add_Image_Int'] = "yes"
        ret['Add_Image_LL_Int'] = "yes"
        ret['Add_Image_Based_Int'] = "yes"
        ret['Add_Image_LL_Based_Int'] = "yes"
        ret['Add_Image_Decimal'] = "yes"
        ret['Add_Image_LL_Decimal'] = "yes"
        ret['Add_Image_Fixed'] = "yes"
        ret['Add_Image_LL_Fixed'] = "yes"
        ret['Add_Image_Float'] = "yes"
        ret['Add_Image_Char'] = "yes"
        ret['Add_Image_Util'] = "yes"

        ret['Add_Value_Bool'] = "yes"
        ret['Add_Value_Enum'] = "yes"
        ret['Add_Value_Decimal'] = "yes"
        ret['Add_Value_LL_Decimal'] = "yes"
        ret['Add_Value_Fixed'] = "yes"
        ret['Add_Value_LL_Fixed'] = "yes"
        ret['Add_Value_Float'] = "yes"
        ret['Add_Value_Int'] = "yes"
        ret['Add_Value_LL_Int'] = "yes"
        ret['Add_Value_Char'] = "yes"
        ret['Add_Value_Util'] = "yes"

        if self.config.is_64bit:
            ret['Add_Exponent_LLL_Int'] = "yes"

            ret['Add_Image_LLL_Int'] = "yes"
            ret['Add_Image_LLL_Based_Int'] = "yes"
            ret['Add_Image_LLL_Decimal'] = "yes"
            ret['Add_Image_LLL_Fixed'] = "yes"

            ret['Add_Value_LLL_Int'] = "yes"
            ret['Add_Value_LLL_Decimal'] = "yes"
            ret['Add_Value_LLL_Fixed'] = "yes"

        if self.config.target is not None:
            cpu = self.config.target.split('-')[0]

            if cpu in ('aarch64', ):
                ret['CPU_Family'] = 'aarch64'
                ret['Has_FMA'] = 'yes'
            elif cpu in ('arm', ):
                ret['CPU_Family'] = 'arm'
                ret['Has_FMA'] = 'no'
            elif cpu.startswith('leon'):
                ret['CPU_Family'] = 'leon'
                ret['Has_FMA'] = 'no'
            elif cpu in ('powerpc', 'ppc'):
                ret['CPU_Family'] = 'powerpc'
                ret['Has_FMA'] = 'yes'
            elif cpu in ('powerpc64', 'ppc64'):
                ret['CPU_Family'] = 'powerpc64'
                ret['Has_FMA'] = 'yes'
            elif cpu in ('x86',):
                ret['CPU_Family'] = 'x86'
                ret['Has_FMA'] = 'no'
            elif cpu in ('x86_64',):
                ret['CPU_Family'] = 'x86_64'
                ret['Has_FMA'] = 'no'
            elif cpu in ('riscv32',):
                ret['CPU_Family'] = 'riscv32'
                ret['Has_FMA'] = 'yes'
            elif cpu in ('riscv64',):
                ret['CPU_Family'] = 'riscv64'
                ret['Has_FMA'] = 'yes'
            else:
                print("Unexpected cpu %s" % cpu)
                sys.exit(2)

        if self.config.has_fpu:
            ret['Has_FPU'] = 'yes'
        else:
            ret['Has_FPU'] = 'no'

        if self.config.has_libc(profile):
            ret['Has_libc'] = 'yes'
        else:
            ret['Has_libc'] = 'no'

        if self.config.use_certifiable_packages:
            ret['Certifiable_Packages'] = 'yes'
        else:
            ret['Certifiable_Packages'] = 'no'

        if self.config.has_single_precision_fpu:
            if self.config.has_double_precision_fpu:
                # Full hardware
                ret['Add_Math_Lib'] = 'hardfloat'
            else:
                # Hardware only for SP.
                ret['Add_Math_Lib'] = 'hardfloat_sp'
        else:
            if self.config.has_double_precision_fpu:
                # Hardware only for DP.
                ret['Add_Math_Lib'] = 'hardfloat_dp'
            else:
                # No hardware support
                ret['Add_Math_Lib'] = 'softfloat'

        if self.config.use_semihosting_io:
            ret['Text_IO'] = 'semihosting'
        else:
            ret['Text_IO'] = 'serial'

        if not self.config.is_pikeos:
            if self.config.has_small_memory:
                ret['Memory_Profile'] = 'small'
            elif self.config.has_huge_memory:
                ret['Memory_Profile'] = 'huge'
            else:
                ret['Memory_Profile'] = 'large'

        # 64-bit specific packages
        if self.config.is_64bit:
            ret['Add_Arith128'] = "yes"
            ret['Target_Word_Size'] = "64"
        else:
            ret['Target_Word_Size'] = "32"

        return ret

    def sfp_scenarios(self, profile='ravenscar-sfp'):
        """Returns the list of directories contained in a base SFP runtime"""
        ret = self.zfp_scenarios(profile)
        ret['RTS_Profile'] = 'ravenscar-sfp'

        if self.config.has_timer_64:
            ret['Timer'] = 'timer64'
        else:
            ret['Timer'] = 'timer32'

        if self.config.is_pikeos:
            ret['Pikeos_Version'] = self.config.pikeos_version

        ret['Has_Compare_And_Swap'] = \
            'yes' if self.config.has_compare_and_swap else 'no'

        return ret

    def full_scenarios(self):
        """Returns the list of directories contained in a base full runtime"""
        ret = self.sfp_scenarios('ravenscar-full')

        # override the RTS value
        ret['RTS_Profile'] = 'ravenscar-full'
        ret['Add_Complex_Type_Support'] = 'yes'
        ret['Add_Image_Wide_Char'] = "yes"
        ret['Add_Pack'] = "yes"
        ret['Add_Streams'] = "yes"
        ret['Add_Traceback'] = "yes"
        ret['Add_Value_Wide_Char'] = "yes"

        # 64-bit specific packages
        if self.config.is_64bit:
            ret['Add_Pack64'] = "yes"

        # We don't support certifiable components with ravenscar-full since we
        # our libgcc replacement does not provide exception support.
        ret['Certifiable_Packages'] = "no"

        if not self.config.is_pikeos:
            # PikeOS provides its own C library
            # ravenscar-full requires C memory operations, either via newlib
            # or via our own implementation in Ada
            ret['Add_C_Integration'] = "newlib"

        return ret

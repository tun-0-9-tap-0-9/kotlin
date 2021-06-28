/*
 * Copyright 2010-2021 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE file.
 */

#include "StackTrace.hpp"

#include "gmock/gmock.h"
#include "gtest/gtest.h"

#include "Common.h"
#include "Porting.h"
#include "TestSupport.hpp"

using namespace kotlin;

namespace {

NO_INLINE void AbortWithStackTrace() {
    PrintStackTraceStderr();
    konan::abort();
}

} // namespace

TEST(StackTraceDeathTest, PrintStackTrace) {
    EXPECT_DEATH(
            { kotlin::RunInNewThread(AbortWithStackTrace); },
            testing::AllOf(testing::HasSubstr("AbortWithStackTrace"), testing::Not(testing::HasSubstr("PrintStackTraceStderr"))));
}

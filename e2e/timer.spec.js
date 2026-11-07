import { test, expect } from '@playwright/test';

async function fillDate(page, { year = '2024', month = '2', day = '29' } = {}) {
  await page.getByLabel('Year', { exact: true }).fill(year);
  await page.getByLabel('Month', { exact: true }).fill(month);
  await page.getByLabel('Day', { exact: true }).fill(day);
}

async function noOverflow(page) {
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= window.innerWidth)).toBe(true);
}

function legacyLink(seconds, title) {
  return '/t/' + Buffer.from(`${seconds}<|>${title}`).toString('base64');
}

test('Given a valid event, when created, then its share link survives reload and back navigation', async ({ page }) => {
  const errors = [];
  page.on('pageerror', error => errors.push(error.message));
  await page.goto('/');
  await noOverflow(page);
  await page.getByRole('link', { name: 'Create a timer.' }).click();
  await expect(page).toHaveURL(/\/new$/);
  await page.getByLabel('Title', { exact: true }).fill('Привет 🎉 <|> / +');
  await fillDate(page);
  // Clearing fields must remain possible; typing must not clamp partial input.
  await page.getByLabel('Year', { exact: true }).fill('');
  await page.getByLabel('Year', { exact: true }).pressSequentially('2024');
  await page.getByRole('button', { name: 'Create timer' }).click();
  await expect(page).toHaveURL(/\/t\/[A-Za-z0-9_-]+$/);
  await expect(page.getByRole('heading', { level: 1 })).toHaveText('Привет 🎉 <|> / +');
  await expect(page.getByText('Time elapsed', { exact: true })).toBeVisible();
  await page.reload();
  await expect(page.getByRole('heading', { level: 1 })).toHaveText('Привет 🎉 <|> / +');
  await noOverflow(page);
  await page.goBack();
  await expect(page.getByRole('heading', { level: 1 })).toHaveText('Create a timer');
  expect(errors).toEqual([]);
});

test('Given an incomplete or impossible date, when submitted, then errors appear and correction succeeds', async ({ page }) => {
  await page.goto('/new');
  await page.getByRole('button', { name: 'Create timer' }).click();
  await expect(page.getByRole('alert')).toHaveText('Give your timer a name.');
  await page.getByLabel('Title', { exact: true }).fill('Leap day');
  await page.getByRole('button', { name: 'Create timer' }).click();
  await expect(page.getByRole('alert')).toContainText('Enter a year, month and day');
  await fillDate(page, { year: '2023' });
  await page.getByRole('button', { name: 'Create timer' }).click();
  await expect(page.getByRole('alert')).toContainText('That date does not exist');
  await expect(page).toHaveURL(/\/new$/);
  await noOverflow(page);
  await page.screenshot({ path: `test-results/form-${test.info().project.name}.png`, fullPage: true });
  await page.getByLabel('Year', { exact: true }).fill('2024');
  await page.getByRole('button', { name: 'Create timer' }).click();
  await expect(page.getByRole('heading', { level: 1 })).toHaveText('Leap day');
});

test('Given old links with padding and slash, when opened directly, then they still display', async ({ page }) => {
  const title = 'Old timer ???';
  const link = legacyLink(1572566400, title);
  expect(link.slice(3)).toContain('/');
  await page.goto(link);
  await expect(page.getByRole('heading', { level: 1 })).toHaveText(title);
  await expect(page.getByText('Time elapsed', { exact: true })).toBeVisible();
});

test('Given a malformed timer link, when opened, then recovery is available', async ({ page }) => {
  for (const link of ['/t/!invalid!', legacyLink('oops', 'Bad timestamp'), '/missing']) {
    await page.goto(link);
    await expect(page.getByRole('alert')).toHaveText('Invalid timer link.');
  }
  await page.getByRole('link', { name: 'New timer' }).click();
  await expect(page.getByRole('heading', { level: 1 })).toHaveText('Create a timer');
});

test('Given a future timer, when clock crosses the event, then countdown becomes elapsed time', async ({ page }) => {
  await page.clock.install({ time: new Date('2024-03-01T00:00:00Z') });
  await page.clock.setFixedTime(new Date('2024-03-01T00:00:00Z'));
  await page.goto(legacyLink(1709251202, 'Soon'));
  await expect(page.getByText('Time remaining', { exact: true })).toBeVisible();
  await expect(page.getByText('2 seconds', { exact: true })).toBeVisible();
  await page.clock.setFixedTime(new Date('2024-03-01T00:00:02Z'));
  await page.clock.runFor(1000);
  await expect(page.getByText('0 seconds', { exact: true })).toBeVisible();
  await expect(page.getByText('Time elapsed', { exact: true })).toBeVisible();
  await page.clock.setFixedTime(new Date('2024-03-01T00:00:03Z'));
  await page.clock.runFor(1000);
  await expect(page.getByText('1 second', { exact: true })).toBeVisible();
});

import 'reflect-metadata'
import { afterAll, beforeAll } from 'bun:test'
import { mkdirSync, rmSync, existsSync } from 'fs'
import { join } from 'path'

const TEST_TEMP_DIR = join(process.cwd(), '.tmp')

export function getTestOutputPath(filename: string): string {
	return join(TEST_TEMP_DIR, filename)
}

beforeAll(() => {
	process.env.NODE_ENV = 'test'

	if (!existsSync(TEST_TEMP_DIR)) {
		mkdirSync(TEST_TEMP_DIR, { recursive: true })
	}
})

afterAll(() => {
	delete process.env.NODE_ENV

	if (existsSync(TEST_TEMP_DIR)) {
		rmSync(TEST_TEMP_DIR, { recursive: true, force: true })
	}
})
